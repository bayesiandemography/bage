
## const ----------------------------------------------------------------------

test_that("'const' works with bage_prior_ar", {
  prior <- AR(n = 3)
  ans_obtained <- const(prior)
  ans_expected <- prior$const
  expect_identical(ans_obtained, ans_expected)
})

test_that("'has_hyperrand' returns TRUE with prior with hyperrand", {
  prior <- compose_time(trend = Lin(), season = Seas(n = 3))
  ans_obtained <- const(prior)
  ans_expected <- unlist(c(list(trend = const(Lin())),
                           list(seasonal = const(Seas(n = 3)))))
  expect_identical(ans_obtained, ans_expected)
})


## draw_vals_effect --------------------------------------------------------------

test_that("'draw_vals_effect' works with bage_prior_ar", {
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

test_that("'draw_vals_effect' works with bage_prior_compose", {
  prior <- compose_time(trend = RW(), error = N())
  agesex <- "other"
  levels_effect <- letters
  matrix_along_by <- matrix(0:25, nr = 26)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        levels_effect = levels_effect,
                                        agesex = agesex,
                                        matrix_agesex = NULL,
                                        matrix_along_by = matrix_along_by,
                                        n_sim = n_sim)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = agesex,
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = NULL,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_ear", {
  prior <- EAR(n = 3)
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

test_that("'draw_vals_effect' works with bage_prior_elin", {
  prior <- ELin(s = 0.01)
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 1:4
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

test_that("'draw_vals_effect' works with bage_prior_erw", {
  prior <- ERW(s = 0.01)
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

test_that("'draw_vals_effect' works with bage_prior_eseas", {
  prior <- ESeas(n = 2, s = 0.01)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters[1:12]
  matrix_along_by = matrix(0:11, nc = 2)
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

test_that("'draw_vals_effect' works with bage_prior_lin", {
  prior <- Lin()
  n_sim <- 10
  vals_hyperrand <- list()
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  levels_effect <- letters
  matrix_along_by = matrix(0:25, nc = 2)
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

test_that("'draw_vals_effect' works with bage_prior_rw", {
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

test_that("'draw_vals_effect' works with bage_prior_rw2", {
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

test_that("'draw_vals_effect' works with bage_prior_seas", {
  prior <- Seas(n = 4)
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

test_that("'draw_vals_effect' works with bage_prior_spline", {
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
  matrix_along_by <- NULL
  matrix_agesex <- make_matrix_along_by(i_along = 1:2,
                                        dim = c(81L, 2L),
                                        dimnames = list(levels_age, levels_sex))
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_age,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "sex:age",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = matrix_agesex,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(162L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_esvd", {
  prior <- ESVD(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_age <- c(0:79, "80+")
  levels_region <- c("A", "B")
  levels_sex <- c("F", "M")
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
                          levels_effect = levels_age,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "age:other",
                          matrix_along_by = matrix_along_by,
                          matrix_agesex = matrix_agesex,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(levels_effect, as.character(1:10)))
})


## draw_vals_hyper ------------------------------------------------------------

test_that("'draw_vals_hyper' works with bage_prior_ar", {
  prior <- AR(n = 3)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("coef", "sd"))
  expect_identical(dim(ans$coef), c(3L, 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_compose", {
  prior <- compose_time(trend = Lin(), cyclical = AR(), seasonal = Seas(n = 2))
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("trend", "cyclical", "seasonal"))
  expect_identical(length(ans$trend$slope), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_ear", {
  prior <- EAR(n = 3)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("coef", "sd"))
  expect_identical(dim(ans$coef), c(3L, 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_elin", {
  set.seed(0)
  prior <- ELin()
  n_sim <- 10
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = n_sim)
  expect_identical(names(ans), c("slope", "sd", "msd"))
  expect_identical(lengths(ans),
                   c(slope = 10L, sd = 10L, msd = 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_erw", {
  set.seed(0)
  prior <- ERW()
  n_sim <- 10
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = n_sim)
  expect_identical(names(ans), "sd")
  expect_identical(lengths(ans), c(sd = 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_eseas", {
  set.seed(0)
  prior <- ESeas(n = 2)
  n_sim <- 10
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = n_sim)
  expect_identical(names(ans), "sd")
  expect_identical(lengths(ans),
                   c(sd = 10L))
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
  expect_identical(names(ans), c("slope", "sd"))
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

test_that("'draw_vals_hyper' works with bage_prior_rw2", {
  prior <- RW2()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_seas", {
  prior <- Seas(n = 4)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
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

test_that("'draw_vals_hyper' works with bage_prior_esvd", {
  prior <- ESVD(HMD)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(ans, list())
})


## 'draw_vals_hyperrand' ------------------------------------------------------

test_that("'draw_vals_hyperrand' works with bage_prior_compose", {
  set.seed(0)
  prior <- compose_time(trend = ELin(), seasonal = ESeas(n = 2))
  levels_effect <- letters[1:12]
  agesex <- "other"
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 1:4
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             levels_effect = levels_effect,
                             agesex = agesex,
                             matrix_along_by = matrix_along_by,
                             matrix_agesex = "other",
                             n_sim = n_sim)
  expect_identical(names(ans), c("trend", "seasonal"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_elin", {
  set.seed(0)
  prior <- ELin()
  levels_effect <- letters[1:12]
  agesex <- "other"
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 1:4
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             levels_effect = levels_effect,
                             agesex = agesex,
                             matrix_along_by = matrix_along_by,
                             matrix_agesex = "other",
                             n_sim = n_sim)
  expect_identical(names(ans), "mslope")
  expect_identical(lengths(ans),
                   c(mslope = 40L))
})

test_that("'draw_vals_hyperrand' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  levels_effect <- letters[1:12]
  agesex <- "other"
  matrix_along_by <- matrix(0:11, nr = 12)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             levels_effect = levels_effect,
                             agesex = agesex,
                             matrix_along_by = matrix_along_by,
                             matrix_agesex = "other",
                             n_sim = n_sim)
  expect_identical(ans, list())
})


## forecast_effect ------------------------------------------------------------

test_that("'forecast_effect' gives correct error with prior for which method does not exist", {
  set.seed(0)
  prior <- Known(value = 1:5)
  effect_est <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = c(-2, -1, 0, 1, 2))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  expect_error(forecast_effect(prior = prior,
                                  nm_prior = "year",
                                  hyper_est =  NULL,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                               levels_forecast = levels_forecast),
               "Can't forecast term \"year\".")
})

test_that("'forecast_effect' works with bage_prior_ar", {
  set.seed(0)
  prior <- AR(n = 3)
  hyper_est <- tibble::tibble(term = "year",
                              component = "hyper",
                              level = c("coef", "coef", "coef", "sd"),
                              .fitted = rvec::runif_rvec(n = 4, n_draw = 10))
  effect_est <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  coef <- hyper_est$.fitted[hyper_est$level == "coef"]
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * effect_est$.fitted[3:5]),
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * c(effect_est$.fitted[4:5],
                                                                  ans_expected$.fitted[1])),
                                              sd = sd)
  ans_expected$.fitted[3] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * c(effect_est$.fitted[5],
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


test_that("'forecast_effect' works with bage_prior_ear", {
  set.seed(0)
  prior <- EAR(n = 2)
  hyper_est <- tibble::tibble(term = "year:reg",
                              component = "hyper",
                              level = c("coef", "coef", "sd"),
                              .fitted = rvec::runif_rvec(n = 3, n_draw = 10))
  effect_est <- tibble::tibble(term = "year:reg",
                               component = "effect",
                               level = letters[1:10],
                               .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10))
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  levels_forecast <- letters[11:22]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year:reg",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = letters[11:22])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  coef <- hyper_est$.fitted[hyper_est$level == "coef"]
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * effect_est$.fitted[4:5]),
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * c(effect_est$.fitted[5],
                                                                  ans_expected$.fitted[1])),
                                              sd = sd)
  ans_expected$.fitted[3] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[1:2]),
                                              sd = sd)
  ans_expected$.fitted[4] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[2:3]),
                                              sd = sd)
  ans_expected$.fitted[5] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[3:4]),
                                              sd = sd)
  ans_expected$.fitted[6] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[4:5]),
                                              sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * effect_est$.fitted[9:10]),
                                              sd = sd)
  ans_expected$.fitted[8] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * c(effect_est$.fitted[10],
                                                                  ans_expected$.fitted[7])),
                                              sd = sd)
  ans_expected$.fitted[9] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[7:8]),
                                              sd = sd)
  ans_expected$.fitted[10] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[8:9]),
                                              sd = sd)
  ans_expected$.fitted[11] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[9:10]),
                                              sd = sd)
  ans_expected$.fitted[12] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(coef * ans_expected$.fitted[10:11]),
                                              sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_effect' works with bage_prior_elin", {
  set.seed(0)
  prior <- ELin()
  hyper_est <- tibble::tibble(term = "year:reg",
                              component = "hyper",
                              level = c("mslope", "mslope", "sd"),
                              .fitted = rvec::runif_rvec(n = 3, n_draw = 10))
  effect_est <- tibble::tibble(term = "year:reg",
                               component = "effect",
                               level = letters[1:10],
                               .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10))
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  levels_forecast <- letters[11:22]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year:reg",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = letters[11:22])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  mslope <- hyper_est$.fitted[hyper_est$level == "mslope"]
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  q <- seq(from = 1.5, by = 0.5, length.out = 6)
  ans_expected$.fitted[1:6] <- rvec::rnorm_rvec(n = 6,
                                                mean = q * mslope[1],
                                                sd = sd)
  ans_expected$.fitted[7:12] <- rvec::rnorm_rvec(n = 6,
                                                 mean = q * mslope[2],
                                                 sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_effect' works with bage_prior_erw", {
  set.seed(0)
  prior <- ERW()
  hyper_est <- tibble::tibble(term = "year:reg",
                              component = "hyper",
                              level = "sd",
                              .fitted = rvec::runif_rvec(n = 1, n_draw = 10))
  effect_est <- tibble::tibble(term = "year:reg",
                               component = "effect",
                               level = letters[1:10],
                               .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10))
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  levels_forecast <- letters[11:22]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year:reg",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = letters[11:22])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[5],
                                              sd = sd)
  for (i in 2:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[10],
                                              sd = sd)
  for (i in 8:12)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_effect' works with bage_prior_eseas", {
  set.seed(0)
  prior <- ESeas(n = 2)
  hyper_est <- tibble::tibble(term = "year:reg",
                              component = "hyper",
                              level = "sd",
                              .fitted = rvec::runif_rvec(n = 1, n_draw = 10))
  effect_est <- tibble::tibble(term = "year:reg",
                               component = "effect",
                               level = letters[1:10],
                               .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10))
  matrix_along_by_est <- matrix(0:9, nr = 5)
  matrix_along_by_forecast <- matrix(0:11, nr = 6)
  levels_forecast <- letters[11:22]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year:reg",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "seasonal",
                                 level = letters[11:22])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[4],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[5],
                                              sd = sd)
  for (i in 3:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-2],
                                                sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[9],
                                              sd = sd)
  ans_expected$.fitted[8] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[10],
                                              sd = sd)
  for (i in 9:12)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-2],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_effect' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  hyper_est <- tibble::tibble(term = "year",
                              component = "hyper",
                              level = c("slope", "sd"),
                              .fitted = rvec::runif_rvec(n = 2, n_draw = 10))
  effect_est <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  slope <- hyper_est$.fitted[hyper_est$level == "slope"]
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  q <- seq(from = 1.5, by = 0.5, length.out = 6)
  set.seed(1)
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6,
                                           mean = q * slope,
                                           sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_effect' works with bage_prior_norm", {
  set.seed(0)
  prior <- N()
  hyper_est <- tibble::tibble(term = "year",
                              component = "hyper",
                              level = "sd",
                              .fitted = rvec::runif_rvec(n = 1, n_draw = 10))
  effect_est <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, mean = 0, sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_effect' works with bage_prior_normfixed", {
  set.seed(0)
  prior <- NFix()
  hyper_est <- NULL
  effect_est <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
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

test_that("'forecast_effect' works with bage_prior_rw", {
  set.seed(0)
  prior <- RW()
  hyper_est <- tibble::tibble(term = "year",
                              component = "hyper",
                              level = "sd",
                              .fitted = rvec::runif_rvec(n = 1, n_draw = 10))
  effect_est <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[5],
                                              sd = sd)
  for (i in 2:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_effect' works with bage_prior_rw2", {
  set.seed(0)
  prior <- RW2()
  hyper_est <- tibble::tibble(term = "year",
                              component = "hyper",
                              level = "sd",
                              .fitted = rvec::runif_rvec(n = 1, n_draw = 10))
  effect_est <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * effect_est$.fitted[5] - effect_est$.fitted[4],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[1] -
                                                effect_est$.fitted[5],
                                              sd = sd)
  for (i in 3:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


test_that("'forecast_effect' works with bage_prior_seas", {
  set.seed(0)
  prior <- Seas(n = 4)
  hyper_est <- tibble::tibble(term = "year",
                              component = "hyper",
                              level = "sd",
                              .fitted = rvec::runif_rvec(n = 1, n_draw = 10))
  effect_est <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  matrix_along_by_est <- matrix(0:4, nr = 5)
  matrix_along_by_forecast <- matrix(0:5, nr = 6)
  levels_forecast <- letters[6:11]
  set.seed(1)
  ans_obtained <- forecast_effect(prior = prior,
                                  nm_prior = "year",
                                  hyper_est =  hyper_est,
                                  hyper_forecast = NULL,
                                  compose_est = NULL,
                                  compose_forecast = NULL,
                                  effect_est = effect_est,
                                  matrix_along_by_est = matrix_along_by_est,
                                  matrix_along_by_forecast = matrix_along_by_forecast,
                                  levels_forecast = levels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "seasonal",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- hyper_est$.fitted[hyper_est$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[2],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[3],
                                              sd = sd)
  ans_expected$.fitted[3] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[4],
                                              sd = sd)
  ans_expected$.fitted[4] <- rvec::rnorm_rvec(n = 1,
                                              mean = effect_est$.fitted[5],
                                              sd = sd)
  ans_expected$.fitted[5] <- rvec::rnorm_rvec(n = 1,
                                              mean = ans_expected$.fitted[1],
                                              sd = sd)
  ans_expected$.fitted[6] <- rvec::rnorm_rvec(n = 1,
                                              mean = ans_expected$.fitted[2],
                                              sd = sd)
  expect_equal(ans_obtained, ans_expected)
})


## forecast_hyper -------------------------------------------------------------

test_that("'forecast_hyper' returns NULL with prior where hyper-parameters not forecast", {
  set.seed(0)
  prior <- N()
  hyper_est <- tibble::tibble(term = "year",
                              component = "hyper",
                              level = "sd",
                              .fitted = rvec::runif_rvec(n = 1, n_draw = 10))
  levels_forecast = letters[1:5]
  ans_obtained <- forecast_hyper(prior = prior,
                                 hyper_est = hyper_est,
                                 levels_forecast = levels_forecast)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
})


## 'has_hyperrand' ------------------------------------------------------------

test_that("'has_hyperrand' returns FALSE with prior without hyperrand", {
  prior <- N()
  expect_false(has_hyperrand(prior))
})

test_that("'has_hyperrand' returns TRUE with prior with hyperrand", {
  prior <- compose_time(trend = Lin(), season = Seas(n = 3))
  expect_true(has_hyperrand(prior))
  prior <- ELin()
  expect_true(has_hyperrand(prior))
})


## 'indices_priors' -----------------------------------------------------------

test_that("'indices_priors' works with non-compose prior", {
  prior <- Lin()
  matrix_along_by <- matrix(0:9, nr = 10)
  ans_obtained <- indices_priors(prior = prior,
                                 matrix_along_by = matrix_along_by)
  expect_identical(ans_obtained, integer())                                 
})

test_that("'indices_priors' works with compose time prior - 2 priors", {
  prior <- compose_time(trend = ELin(),
                        cyclical = EAR())
  matrix_along_by <- matrix(0:9, nr = 5, dimnames = list(a = 1:5, b = 1:2))
  ans_obtained <- indices_priors(prior = prior,
                                 matrix_along_by = matrix_along_by)
  ans_expected <- c(hyper_start = 0L,
                    hyper_length = 3L,
                    hyperrand_start = 0L,
                    hyperrand_length = 12L,
                    consts_start = 0L,
                    consts_length = 3L,
                    i_prior = 9L,
                    hyper_start = 3L,
                    hyper_length = 3L,
                    hyperrand_start = 12L,
                    hyperrand_length = 0L,
                    consts_start = 3L,
                    consts_length = 5L,
                    i_prior = 12L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'indices_priors' works with compose time prior - 3 priors", {
  prior <- compose_time(trend = RW2(),
                        cyclical = AR(),
                        error = N())
  matrix_along_by <- matrix(0:9, nr = 10)
  rownames(matrix_along_by) <- 1:10
  names(dimnames(matrix_along_by))[1] <- "x"
  ans_obtained <- indices_priors(prior = prior,
                                 matrix_along_by = matrix_along_by)
  ans_expected <- c(hyper_start = 0L,
                    hyper_length = 1L,
                    hyperrand_start = 0L,
                    hyperrand_length = 10L,
                    consts_start = 0L,
                    consts_length = 2L,
                    i_prior = 4L,
                    hyper_start = 1L,
                    hyper_length = 3L,
                    hyperrand_start = 10L,
                    hyperrand_length = 10L,
                    consts_start = 2L,
                    consts_length = 5L,
                    i_prior = 5L,
                    hyper_start = 4L,
                    hyper_length = 1L,
                    hyperrand_start = 20L,
                    hyperrand_length = 0L,
                    consts_start = 7L,
                    consts_length = 1L,
                    i_prior = 1L)
  expect_identical(ans_obtained, ans_expected)
})


## is_known -------------------------------------------------------------------

test_that("'is_known' works with valid inputs", {
    expect_false(is_known(N()))
    expect_true(is_known(Known(values = 1:3)))
})


## is_prior_ok_for_term -------------------------------------------------------

test_that("'is_prior_ok_for_term' works with bage_prior_ar1", {
    expect_true(is_prior_ok_for_term(prior = AR1(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:3, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_compose - time", {
  prior <- compose_time(trend = RW2(),
                        cyclical = AR(),
                        seas = Seas(n = 4))
  expect_true(is_prior_ok_for_term(prior = prior,
                                   nm = "time",
                                   matrix_along_by = matrix(0:9, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
  expect_error(is_prior_ok_for_term(prior = prior,
                                    nm = "bla:blu",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = TRUE,
                                    agesex = "other"),
               "Problem with call to `bage::compose_time\\(\\)`.")
})

test_that("'is_prior_ok_for_term' works with bage_prior_ear", {
    expect_true(is_prior_ok_for_term(prior = EAR(n = 3),
                                     nm = "time:region",
                                     matrix_along_by = matrix(0:29, nc = 3),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_elin", {
    expect_true(is_prior_ok_for_term(prior = ELin(),
                                     nm = "sex:time",
                                     matrix_along_by = matrix(0:11, nc = 2),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_erw", {
  expect_true(is_prior_ok_for_term(prior = ERW(),
                                   nm = "age:time",
                                   matrix_along_by = matrix(0:11, nc = 3),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_eseas", {
  expect_true(is_prior_ok_for_term(prior = ESeas(n = 4),
                                   nm = "time",
                                   matrix_along_by = matrix(0:11, nc = 3),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = TRUE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_eseas", {
  expect_error(is_prior_ok_for_term(prior = ESeas(n = 4),
                                    nm = "time",
                                    matrix_along_by = matrix(0:12, nc = 3),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "`ESeas\\(n=4\\)` prior cannot be used on its own.")
})

test_that("'is_prior_ok_for_term' works with bage_prior_known", {
  expect_true(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                   nm = "sex",
                                   matrix_along_by = matrix(0:1, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_lin", {
  expect_true(is_prior_ok_for_term(prior = Lin(),
                                   nm = "sex",
                                   matrix_along_by = matrix(0:1, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws expected error with bage_prior_known", {
  expect_error(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                    nm = "sex",
                                    matrix_along_by = matrix(0:2, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "`Known\\(c\\(0.1,-0.1\\)\\)` prior for `sex` term invalid.")    
})

test_that("'is_prior_ok_for_term' works with bage_prior_norm", {
    expect_true(is_prior_ok_for_term(prior = N(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_normfixed", {
    expect_true(is_prior_ok_for_term(prior = NFix(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw", {
    expect_true(is_prior_ok_for_term(prior = RW(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2", {
    expect_true(is_prior_ok_for_term(prior = RW2(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:2, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_seas", {
  expect_true(is_prior_ok_for_term(prior = Seas(n = 4),
                                   nm = "time",
                                   matrix_along_by = matrix(0:3, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   is_in_compose = TRUE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_seas", {
  expect_error(is_prior_ok_for_term(prior = Seas(n = 4),
                                    nm = "time",
                                    matrix_along_by = matrix(0:3, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "`Seas\\(n=4\\)` prior cannot be used on its own.")
})

test_that("'is_prior_ok_for_term' works with bage_prior_spline", {
    expect_true(is_prior_ok_for_term(prior = Sp(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
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
                                   is_in_compose = FALSE,
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
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "Problem with `SVD\\(s\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when agesex is 'other'", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "Problem with `SVDS\\(s\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for age main effect", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "age",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age"),
               "Problem with `SVDS\\(s\\)` prior for `age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for age-sex interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age"),
               "Problem with `SVD\\(s\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for age interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "age:reg",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVD\\(s\\)` prior for `age:reg` term.")
})


test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for misclassified age-sex interaction, var_sexgender is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = NULL,
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVDS\\(s\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for misclassified age-sex interaction, var_sexgender is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVDS\\(s\\)` prior for `bla:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when dim has length 2 and 'agesex' not recognised", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "wrong"),
               "Internal error: unexpected value for `agesex`.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for 3-way interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "sex:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age:other"),
               "Problem with `SVDS\\(s\\)` prior for `sex:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for 3-way interaction involving age, sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "sex:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age:other"),
               "Problem with `SVD\\(s\\)` prior for `sex:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for 3-way interaction involving age, not sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVDS\\(s\\)` prior for `reg:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for 3-way interaction involving age, not sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVD\\(s\\)` prior for `reg:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when 2+ dimensions and invalid 'agesex'", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "wrong"),
               "Internal error: unexpected value for `agesex`.")
})


## levels_hyper ---------------------------------------------------------------

test_that("'levels_hyper' works with 'bage_prior_ar'", {
  expect_identical(levels_hyper(prior = AR(n = 2)),
                   c("coef1", "coef2", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_compose - time'", {
  prior <- compose_time(trend = Lin(), cyclical = AR(), seasonal = Seas(n = 4))
  expect_identical(levels_hyper(prior),
                   unname(c(trend = paste("trend", levels_hyper(Lin()), sep = "."),
                            cyclical = paste("cyclical", levels_hyper(AR()), sep = "."),
                            seasonal = paste("seasonal", levels_hyper(Seas(n = 4)), sep = "."))))
})

test_that("'levels_hyper' works with 'bage_prior_ear'", {
  expect_identical(levels_hyper(prior = EAR(n = 2)),
                   c("coef", "coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_elin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = ELin()),
                   c("slope", "sd", "msd"))
})

test_that("'levels_hyper' works with 'bage_prior_erw'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = ERW()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_eseas'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = ESeas(n = 3)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_known'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Known(1)),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_lin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Lin()),
                   c("slope", "sd"))
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
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = RW()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = RW2()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_seas'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Seas(n = 2)),
                   "sd")
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

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 2 priors", {
  prior <- compose_time(ELin(), seasonal = ESeas(n = 4))
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = prior,
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c(paste("trend.effect", levels_effect, sep = "."),
                    c("trend.mslope.a", "trend.mslope.b"))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 3 priors", {
  prior <- compose_time(ELin(), seasonal = ESeas(n = 4), cyclical = EAR())
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = prior,
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c(paste("trend.effect", levels_effect, sep = "."),
                    c("trend.mslope.a", "trend.mslope.b"),
                    paste("cyclical.effect", levels_effect, sep = "."))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 4 priors", {
  prior <- compose_time(ELin(), seasonal = ESeas(n = 4), cyclical = EAR(), error = N())
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = prior,
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c(paste("trend.effect", levels_effect, sep = "."),
                     c("trend.mslope.a", "trend.mslope.b"),
                     paste("cyclical.effect", levels_effect, sep = "."),
                     paste("seasonal.effect", levels_effect, sep = "."))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_elin'", {
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = ELin(),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c("mslope.a", "mslope.b")
  expect_identical(ans_obtained, ans_expected)                   
})


## 'make_matrix_effectfree_effect' --------------------------------------------------

test_that("'make_matrix_effectfree_effect' works with bage_prior_ar1", {
  prior <- AR1()
  levels_effect <- 2001:2005
  agesex <- "other"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = NULL,
                                                levels_sexgender = NULL,
                                                agesex = agesex)
  ans_expected <- Matrix::.sparseDiagonal(5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - n supplied", {
  prior <- Sp(n = 5)
  levels_effect <- 1:10
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = 0:9,
                                                levels_sexgender = NULL,
                                                agesex = agesex)
  ans_expected <- make_spline_matrix(length_effect = 10,
                                     n_spline = 5)
  rownames(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - n NULL", {
  prior <- Sp()
  levels_effect <- 1:10
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = 0:9,
                                                levels_sexgender = NULL,
                                                agesex = agesex)
  ans_expected <- make_spline_matrix(length_effect = 10,
                                     n_spline = 7)
  rownames(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n = 3)
  levels_effect <- c("0-4", "5-9")
  agesex <- "age"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_effect,
                                                levels_sexgender = NULL,
                                                agesex = agesex)
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
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_age,
                                                levels_sex = levels_sex,
                                                agesex = agesex)
  ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][c(1,3,2,4),1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected),
                                       dimnames = dimnames(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_esvd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- ESVD(ssvd = s, n = 3)
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
                                                matrix_agesex = matrix_agesex)
  m2 <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_matrix_agesex_index(matrix_agesex)
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_esvd - sex x reg x age interaction", {
  prior <- ESVDS(HMD)
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
                                                matrix_agesex = matrix_agesex)
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:5, 11:15)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_matrix_agesex_index(matrix_agesex)
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
                                                agesex = agesex)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVDS(ssvd = s, n = 3, joint = TRUE)
  agesex <- "sex:age"
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                levels_age = c("0-4", "5-9"),
                                                levels_sexgender = c("Female", "Male"),
                                                agesex = agesex)
  ans_expected <- s$data$offset[s$data$type == "joint"][[1L]][c(1,3,2,4)]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_esvd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- ESVD(ssvd = s, n = 3)
  levels_effect = c("a.0-4", "b.0-4", "a.5-9", "b.5-9")
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
  m <- make_matrix_agesex_index(matrix_agesex)
  ans_expected <- Matrix::drop(m %*% off)
  expect_identical(ans_obtained, ans_expected)
})

## const ----------------------------------------------------------------------

test_that("'const' works with bage_prior_ar", {
  prior <- AR(n = 3)
  ans_obtained <- const(prior)
  ans_expected <- prior$const
  expect_identical(ans_obtained, ans_expected)
})

test_that("'has_hyperrand' returns TRUE with prior with hyperrand", {
  prior <- compose_time(trend = Lin(), season = Seas(n = 3))
  ans_obtained <- const(prior)
  ans_expected <- unlist(c(list(trend = const(Lin())),
                           list(seasonal = const(Seas(n = 3)))))
  expect_identical(ans_obtained, ans_expected)
})


## draw_vals_effect --------------------------------------------------------------

test_that("'draw_vals_effect' works with bage_prior_ar", {
  prior <- AR(n = 3)
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
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_compose", {
  prior <- compose_time(trend = RW(), error = N())
  agesex <- "other"
  levels_effect <- letters
  matrix_along_by <- matrix(0:25, nr = 26)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        levels_effect = levels_effect,
                                        agesex = agesex,
                                        matrix_along_by = matrix_along_by,
                                        matrix_agesex = NULL,
                                        n_sim = n_sim)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = agesex,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_ear", {
  prior <- EAR(n = 3)
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
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_elin", {
  prior <- ELin(s = 0.01)
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 1:4
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
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_erw", {
  prior <- ERW(s = 0.01)
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
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_eseas", {
  prior <- ESeas(n = 2, s = 0.01)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- letters[1:12]
  matrix_along_by = matrix(0:11, nc = 2)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_known", {
  prior <- Known(c(-0.1, 0, 0.1))
  n_sim <- 10
  vals_hyperrand <- list()
  levels_effect <- c("a", "b", "c")
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = NULL,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(c("a", "b", "c"), as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_lin", {
  prior <- Lin()
  n_sim <- 10
  vals_hyperrand <- list()
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
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
  ans <- draw_vals_effect(prior = prior,
                          vals_hyperrand = vals_hyperrand,
                          vals_hyper = vals_hyper,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(as.character(1:1000), as.character(1:10)))
  expect_equal(unname(apply(ans, 2, sd)), vals_hyper$sd, tolerance = 0.05)
})

test_that("'draw_vals_effect' works with bage_prior_normfixed", {
  prior <- NFix(sd = 0.3)
  n_sim <- 10
  vals_hyperrand <- list()
  levels_effect <- seq_len(1000)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = NULL,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "other",
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(as.character(1:1000), as.character(1:10)))
  expect_equal(unname(apply(ans, 2, sd)), rep(0.3, 10), tolerance = 0.05)
})

test_that("'draw_vals_effect' works with bage_prior_rw", {
  prior <- RW()
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
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rw2", {
  prior <- RW2()
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
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_seas", {
  prior <- Seas(n = 4)
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
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_spline", {
  prior <- Sp()
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
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age main effect", {
  prior <- SVD(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_age <- c(0:79, "80+")
  levels_sex <- c("F", "M")
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_age,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "age",
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
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_age,
                          levels_age = levels_age,
                          levels_sex = levels_sex,
                          agesex = "sex:age",
                          matrix_along_by = matrix_along_by,
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

test_that("'draw_vals_hyper' works with bage_prior_compose", {
  prior <- compose_time(trend = Lin(), cyclical = AR(), seasonal = Seas(n = 2))
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("trend", "cyclical", "seasonal"))
  expect_identical(length(ans$trend$slope), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_ear", {
  prior <- EAR(n = 3)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("coef", "sd"))
  expect_identical(dim(ans$coef), c(3L, 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_elin", {
  set.seed(0)
  prior <- ELin()
  n_sim <- 10
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = n_sim)
  expect_identical(names(ans), c("slope", "sd", "msd"))
  expect_identical(lengths(ans),
                   c(slope = 10L, sd = 10L, msd = 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_erw", {
  set.seed(0)
  prior <- ERW()
  n_sim <- 10
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = n_sim)
  expect_identical(names(ans), "sd")
  expect_identical(lengths(ans), c(sd = 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_eseas", {
  set.seed(0)
  prior <- ESeas(n = 2)
  n_sim <- 10
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = n_sim)
  expect_identical(names(ans), "sd")
  expect_identical(lengths(ans),
                   c(sd = 10L))
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
  expect_identical(names(ans), c("slope", "sd"))
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

test_that("'draw_vals_hyper' works with bage_prior_rw2", {
  prior <- RW2()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_seas", {
  prior <- Seas(n = 4)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
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

test_that("'draw_vals_hyperrand' works with bage_prior_compose", {
  set.seed(0)
  prior <- compose_time(trend = ELin(), seasonal = ESeas(n = 2))
  levels_effect <- letters[1:12]
  agesex <- "other"
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 1:4
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             levels_effect = levels_effect,
                             agesex = agesex,
                             matrix_along_by = matrix_along_by,
                             matrix_agesex = NULL,
                             n_sim = n_sim)
  expect_identical(names(ans), c("trend", "seasonal"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_elin", {
  set.seed(0)
  prior <- ELin()
  levels_effect <- letters[1:12]
  agesex <- "other"
  matrix_along_by <- matrix(0:11, nr = 3)
  colnames(matrix_along_by) <- 1:4
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             levels_effect = levels_effect,
                             agesex = agesex,
                             matrix_along_by = matrix_along_by,
                             matrix_agesex = NULL,
                             n_sim = n_sim)
  expect_identical(names(ans), "mslope")
  expect_identical(lengths(ans),
                   c(mslope = 40L))
})

test_that("'draw_vals_hyperrand' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  levels_effect <- letters[1:12]
  agesex <- "other"
  matrix_along_by <- matrix(0:11, nr = 12)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             levels_effect = levels_effect,
                             agesex = agesex,
                             matrix_along_by = matrix_along_by,
                             matrix_agesex = NULL,
                             n_sim = n_sim)
  expect_identical(ans, list())
})


## 'has_hyperrand' ------------------------------------------------------------

test_that("'has_hyperrand' returns FALSE with prior without hyperrand", {
  prior <- N()
  expect_false(has_hyperrand(prior))
})

test_that("'has_hyperrand' returns TRUE with prior with hyperrand", {
  prior <- compose_time(trend = Lin(), season = Seas(n = 3))
  expect_true(has_hyperrand(prior))
  prior <- ELin()
  expect_true(has_hyperrand(prior))
})


## 'indices_priors' -----------------------------------------------------------

test_that("'indices_priors' works with non-compose prior", {
  prior <- Lin()
  matrix_along_by <- matrix(0:9, nr = 10)
  ans_obtained <- indices_priors(prior = prior,
                                 matrix_along_by = matrix_along_by)
  expect_identical(ans_obtained, integer())                                 
})

test_that("'indices_priors' works with compose time prior - 2 priors", {
  prior <- compose_time(trend = ELin(),
                        cyclical = EAR())
  matrix_along_by <- matrix(0:9, nr = 5, dimnames = list(a = 1:5, b = 1:2))
  ans_obtained <- indices_priors(prior = prior,
                                 matrix_along_by = matrix_along_by)
  ans_expected <- c(hyper_start = 0L,
                    hyper_length = 3L,
                    hyperrand_start = 0L,
                    hyperrand_length = 12L,
                    consts_start = 0L,
                    consts_length = 3L,
                    i_prior = 9L,
                    hyper_start = 3L,
                    hyper_length = 3L,
                    hyperrand_start = 12L,
                    hyperrand_length = 0L,
                    consts_start = 3L,
                    consts_length = 5L,
                    i_prior = 12L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'indices_priors' works with compose time prior - 3 priors", {
  prior <- compose_time(trend = RW2(),
                        cyclical = AR(),
                        error = N())
  matrix_along_by <- matrix(0:9, nr = 10)
  rownames(matrix_along_by) <- 1:10
  names(dimnames(matrix_along_by))[1] <- "x"
  ans_obtained <- indices_priors(prior = prior,
                                 matrix_along_by = matrix_along_by)
  ans_expected <- c(hyper_start = 0L,
                    hyper_length = 1L,
                    hyperrand_start = 0L,
                    hyperrand_length = 10L,
                    consts_start = 0L,
                    consts_length = 2L,
                    i_prior = 4L,
                    hyper_start = 1L,
                    hyper_length = 3L,
                    hyperrand_start = 10L,
                    hyperrand_length = 10L,
                    consts_start = 2L,
                    consts_length = 5L,
                    i_prior = 5L,
                    hyper_start = 4L,
                    hyper_length = 1L,
                    hyperrand_start = 20L,
                    hyperrand_length = 0L,
                    consts_start = 7L,
                    consts_length = 1L,
                    i_prior = 1L)
  expect_identical(ans_obtained, ans_expected)
})


## is_known -------------------------------------------------------------------

test_that("'is_known' works with valid inputs", {
    expect_false(is_known(N()))
    expect_true(is_known(Known(values = 1:3)))
})


## is_prior_ok_for_term -------------------------------------------------------

test_that("'is_prior_ok_for_term' works with bage_prior_ar1", {
    expect_true(is_prior_ok_for_term(prior = AR1(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:3, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_compose - time", {
  prior <- compose_time(trend = RW2(),
                        cyclical = AR(),
                        seas = Seas(n = 4))
  expect_true(is_prior_ok_for_term(prior = prior,
                                   nm = "time",
                                   matrix_along_by = matrix(0:9, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
  expect_error(is_prior_ok_for_term(prior = prior,
                                    nm = "bla:blu",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = TRUE,
                                    agesex = "other"),
               "Problem with call to `bage::compose_time\\(\\)`.")
})

test_that("'is_prior_ok_for_term' works with bage_prior_ear", {
    expect_true(is_prior_ok_for_term(prior = EAR(n = 3),
                                     nm = "time:region",
                                     matrix_along_by = matrix(0:29, nc = 3),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_elin", {
    expect_true(is_prior_ok_for_term(prior = ELin(),
                                     nm = "sex:time",
                                     matrix_along_by = matrix(0:11, nc = 2),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_erw", {
  expect_true(is_prior_ok_for_term(prior = ERW(),
                                   nm = "age:time",
                                   matrix_along_by = matrix(0:11, nc = 3),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_eseas", {
  expect_true(is_prior_ok_for_term(prior = ESeas(n = 4),
                                   nm = "time",
                                   matrix_along_by = matrix(0:11, nc = 3),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = TRUE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_eseas", {
  expect_error(is_prior_ok_for_term(prior = ESeas(n = 4),
                                    nm = "time",
                                    matrix_along_by = matrix(0:12, nc = 3),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "`ESeas\\(n=4\\)` prior cannot be used on its own.")
})

test_that("'is_prior_ok_for_term' works with bage_prior_known", {
  expect_true(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                   nm = "sex",
                                   matrix_along_by = matrix(0:1, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_lin", {
  expect_true(is_prior_ok_for_term(prior = Lin(),
                                   nm = "sex",
                                   matrix_along_by = matrix(0:1, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws expected error with bage_prior_known", {
  expect_error(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                    nm = "sex",
                                    matrix_along_by = matrix(0:2, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "`Known\\(c\\(0.1,-0.1\\)\\)` prior for `sex` term invalid.")    
})

test_that("'is_prior_ok_for_term' works with bage_prior_norm", {
    expect_true(is_prior_ok_for_term(prior = N(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_normfixed", {
    expect_true(is_prior_ok_for_term(prior = NFix(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw", {
    expect_true(is_prior_ok_for_term(prior = RW(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2", {
    expect_true(is_prior_ok_for_term(prior = RW2(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:2, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_seas", {
  expect_true(is_prior_ok_for_term(prior = Seas(n = 4),
                                   nm = "time",
                                   matrix_along_by = matrix(0:3, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   is_in_compose = TRUE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_seas", {
  expect_error(is_prior_ok_for_term(prior = Seas(n = 4),
                                    nm = "time",
                                    matrix_along_by = matrix(0:3, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "`Seas\\(n=4\\)` prior cannot be used on its own.")
})

test_that("'is_prior_ok_for_term' works with bage_prior_spline", {
    expect_true(is_prior_ok_for_term(prior = Sp(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex",
                                     is_in_compose = FALSE,
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
                                   is_in_compose = FALSE,
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
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "Problem with `SVD\\(s\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when agesex is 'other'", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "Problem with `SVDS\\(s\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for age main effect", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "age",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age"),
               "Problem with `SVDS\\(s\\)` prior for `age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for age-sex interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age"),
               "Problem with `SVD\\(s\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for misclassified age-sex interaction, var_sexgender is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = NULL,
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVDS\\(s\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for misclassified age-sex interaction, var_sexgender is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVDS\\(s\\)` prior for `bla:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when dim has length 2 and 'agesex' not recognised", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "bla:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "wrong"),
               "Internal error: unexpected value for `agesex`.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for 3-way interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "sex:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age:other"),
               "Problem with `SVDS\\(s\\)` prior for `sex:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for 3-way interaction involving age, sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "sex:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age:other"),
               "Problem with `SVD\\(s\\)` prior for `sex:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVDS is used for 3-way interaction involving age, not sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVDS(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVDS\\(s\\)` prior for `reg:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when SVD is used for 3-way interaction involving age, not sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `SVD\\(s\\)` prior for `reg:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when 2+ dimensions and invalid 'agesex'", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "wrong"),
               "Internal error: unexpected value for `agesex`.")
})











test_that("'is_prior_ok_for_term' works with bage_prior_esvd, correct inputs", {
  s <- sim_ssvd()
  expect_true(is_prior_ok_for_term(prior = ESVDS(s),
                                   nm = "age:sex:other",
                                   matrix_along_by = NULL,
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex",
                                   is_in_compose = FALSE,
                                   agesex = "age:sex:other"))
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when 'var_age' is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVD(s),
                                    nm = "bla",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = NULL,
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "Problem with `ESVD\\(s\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when agesex is 'other'", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVDS(s),
                                    nm = "bla",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "Problem with `ESVDS\\(s\\)` prior for `bla` term.")
})


test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when ESVDS is used for age main effect", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVDS(s),
                                    nm = "age",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age"),
               "Problem with `ESVDS\\(s\\)` prior for `age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when ESVD is used for age main effect", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVD(s),
                                    nm = "age",
                                    matrix_along_by = matrix(0:9, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age"),
               "Problem with `ESVD\\(s\\)` prior for `age` term.")
})


test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when ESVD is used for age-sex interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVDS(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age"),
               "Problem with `ESVDS\\(s\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when ESVD is used for age-sex interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVD(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age"),
               "Problem with `ESVD\\(s\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when ESVDS is used for misclassified age-sex interaction, var_sexgender is NULL", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVDS(s),
                                    nm = "sex:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = NULL,
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `ESVDS\\(s\\)` prior for `sex:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when ESVDS is used for age-other interaction", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVDS(s),
                                    nm = "bla:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `ESVDS\\(s\\)` prior for `bla:age` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when dim has length 2 and 'agesex' not recognised", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVDS(s),
                                    nm = "bla:age",
                                    matrix_along_by = matrix(0:9, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "wrong"),
               "Internal error: unexpected value for `agesex`.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when ESVD is used for 3-way interaction involving age and sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVD(s),
                                    nm = "sex:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "sex:age:other"),
               "Problem with `ESVD\\(s\\)` prior for `sex:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when ESVDS is used for 3-way interaction involving age, not sex", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVDS(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "age:other"),
               "Problem with `ESVDS\\(s\\)` prior for `reg:age:time` term.")
})

test_that("'is_prior_ok_for_term' method for bage_prior_svd throws correct error when 2+ dimensions and invalid 'agesex'", {
  s <- sim_ssvd()
  expect_error(is_prior_ok_for_term(prior = ESVD(s),
                                    nm = "reg:age:time",
                                    matrix_along_by = matrix(0:29, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex",
                                    is_in_compose = FALSE,
                                    agesex = "wrong"),
               "Internal error: unexpected value for `agesex`.")
})


## levels_hyper ---------------------------------------------------------------

test_that("'levels_hyper' works with 'bage_prior_ar'", {
  expect_identical(levels_hyper(prior = AR(n = 2)),
                   c("coef1", "coef2", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_compose - time'", {
  prior <- compose_time(trend = Lin(), cyclical = AR(), seasonal = Seas(n = 4))
  expect_identical(levels_hyper(prior),
                   unname(c(trend = paste("trend", levels_hyper(Lin()), sep = "."),
                            cyclical = paste("cyclical", levels_hyper(AR()), sep = "."),
                            seasonal = paste("seasonal", levels_hyper(Seas(n = 4)), sep = "."))))
})

test_that("'levels_hyper' works with 'bage_prior_ear'", {
  expect_identical(levels_hyper(prior = EAR(n = 2)),
                   c("coef", "coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_elin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = ELin()),
                   c("slope", "sd", "msd"))
})

test_that("'levels_hyper' works with 'bage_prior_erw'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = ERW()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_eseas'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = ESeas(n = 3)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_known'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Known(1)),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_lin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Lin()),
                   c("slope", "sd"))
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
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = RW()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = RW2()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_seas'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Seas(n = 2)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_spline'", {
  expect_identical(levels_hyper(prior = Sp()), 
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_svd'", {
  expect_identical(levels_hyper(prior = SVD(sim_ssvd())),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_esvd'", {
  expect_identical(levels_hyper(prior = ESVD(sim_ssvd())),
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

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 2 priors", {
  prior <- compose_time(ELin(), seasonal = ESeas(n = 4))
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = prior,
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c(paste("trend.effect", levels_effect, sep = "."),
                    c("trend.mslope.a", "trend.mslope.b"))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 3 priors", {
  prior <- compose_time(ELin(), seasonal = ESeas(n = 4), cyclical = EAR())
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = prior,
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c(paste("trend.effect", levels_effect, sep = "."),
                    c("trend.mslope.a", "trend.mslope.b"),
                    paste("cyclical.effect", levels_effect, sep = "."))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 4 priors", {
  prior <- compose_time(ELin(), seasonal = ESeas(n = 4), cyclical = EAR(), error = N())
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = prior,
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c(paste("trend.effect", levels_effect, sep = "."),
                     c("trend.mslope.a", "trend.mslope.b"),
                     paste("cyclical.effect", levels_effect, sep = "."),
                     paste("seasonal.effect", levels_effect, sep = "."))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_elin'", {
  levels_effect <- paste(letters[1:13], rep(c("a", "b"), each = 2), sep = ".")
  matrix_along_by <- matrix(0:25,
                            nr = 13,
                            dimnames = list(x = letters[1:13], y = c("a", "b")))
  ans_obtained <- levels_hyperrand(prior = ELin(),
                                   matrix_along_by = matrix_along_by,
                                   levels_effect = levels_effect)
  ans_expected <- c("mslope.a", "mslope.b")
  expect_identical(ans_obtained, ans_expected)                   
})


## 'make_matrix_effectfree_effect' --------------------------------------------------

test_that("'make_matrix_effectfree_effect' works with bage_prior_ar1", {
  prior <- AR1()
  levels_effect <- 2001:2005
  agesex <- "other"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = NULL,
                                                levels_sexgender = NULL,
                                                agesex = agesex)
  ans_expected <- Matrix::.sparseDiagonal(5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - n supplied", {
  prior <- Sp(n = 5)
  levels_effect <- 1:10
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = 0:9,
                                                levels_sexgender = NULL,
                                                agesex = agesex)
  ans_expected <- make_spline_matrix(length_effect = 10,
                                     n_spline = 5)
  rownames(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - n NULL", {
  prior <- Sp()
  levels_effect <- 1:10
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = 0:9,
                                                levels_sexgender = NULL,
                                                agesex = agesex)
  ans_expected <- make_spline_matrix(length_effect = 10,
                                     n_spline = 7)
  rownames(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n = 3)
  levels_effect <- c("0-4", "5-9")
  agesex <- "age"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_effect,
                                                levels_sexgender = NULL,
                                                agesex = agesex)
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
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_age,
                                                levels_sex = levels_sex,
                                                agesex = agesex)
  ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][c(1,3,2,4),1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected),
                                       dimnames = dimnames(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_esvd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- ESVD(ssvd = s, n = 3)
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
                                                matrix_agesex = matrix_agesex)
  m2 <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_matrix_agesex_index(matrix_agesex)
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_esvd - sex x reg x age interaction", {
  prior <- ESVDS(HMD)
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
                                                matrix_agesex = matrix_agesex)
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:5, 11:15)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_matrix_agesex_index(matrix_agesex)
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
                                                agesex = agesex)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVDS(ssvd = s, n = 3, joint = TRUE)
  agesex <- "sex:age"
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                levels_age = c("0-4", "5-9"),
                                                levels_sexgender = c("Female", "Male"),
                                                agesex = agesex)
  ans_expected <- s$data$offset[s$data$type == "joint"][[1L]][c(1,3,2,4)]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_esvd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- ESVD(ssvd = s, n = 3)
  levels_effect = c("a.0-4", "b.0-4", "a.5-9", "b.5-9")
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
  m <- make_matrix_agesex_index(matrix_agesex)
  ans_expected <- Matrix::drop(m %*% off)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_esvd - sex x reg x age interaction", {
  prior <- ESVDS(HMD)
  levels_age <- c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+")
  levels_sex <- c("F", "M")
  levels_reg <- c("A", "B")
  levels_effect <- paste(levels_sex, rep(levels_reg, each = 2), rep(levels_age, each = 4), sep = ".")
  agesex <- "sex:age:other"
  matrix_agesex <- make_matrix_along_by(i_along = c(1, 3),
                                        dim = c(2, 2, 14),
                                        dimnames = list(levels_sex, levels_reg, levels_age))
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                levels_age = levels_age,
                                                levels_sexgender = levels_sex,
                                                agesex = agesex,
                                                matrix_agesex = matrix_agesex)
  off <- HMD$data$offset[[35]][as.integer(t(matrix(1:28,nr=14)))]
  off <- c(off, off)
  m1 <- make_matrix_agesex_index(matrix_agesex)
  ans_expected <- Matrix::drop(m1 %*% off)
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
    ans_obtained <- fit(mod)
    matrix_along_by <- choose_matrices_along_by(mod)[[2]]
    comp <- components(mod)
    ans_obtained <- reformat_hyperrand_one(prior = mod$priors[[2]],
                                           nm_prior <- names(mod$priors)[[2]],
                                           matrix_along_by = matrix_along_by,
                                           components = comp)
    ans_expected <- comp
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_hyperrand_one' works with bage_prior_compose - time, two components", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ compose_time(ELin(), error = N())) |>
                  fit(mod)
  mod <- set_n_draw(mod, 5)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  comp <- make_comp_components(mod)
  term <- make_term_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(component = comp,
                               term = term,
                               level = level,
                               .fitted = .fitted)
  ans_obtained <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                         nm_prior = "sex:time",
                                         matrix_along_by = matrix_along_by,
                                         components = components)
  hyperrand_comp <- subset(components,
                           component == "hyperrand" & grepl("trend\\.effect\\.", level))
  effect_comp <- subset(components, component == "effect" & term == "sex:time")
  hyperrand_new <- hyperrand_comp
  hyperrand_new$.fitted <- effect_comp$.fitted - hyperrand_comp$.fitted
  hyperrand_new$level <- sub("trend\\.effect\\.", "", hyperrand_new$level)
  hyperrand_new$component <- "error"
  hyperrand_comp$level <- sub("trend\\.effect\\.", "", hyperrand_new$level)
  hyperrand_comp$component <- "trend"
  hyper <- subset(components,
                  component == "hyperrand" & grepl("mslope", level))
  hyper$component <- "hyper"
  ans_expected <- vctrs::vec_rbind(components[1:32,],
                                   hyper,
                                   hyperrand_comp,
                                   hyperrand_new,
                                   components[47, , drop = FALSE])
  expect_identical(ans_obtained[1:3], ans_expected[1:3])
  expect_equal(as.numeric(mean(subset(ans_obtained, component == "trend" & grepl("M", level))$.fitted)),
               c(0, 0, 0, 0, 0))
})

test_that("'reformat_hyperrand_one' works with bage_prior_compose - time, three components", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ compose_time(ELin(), cyclical = EAR(), error = N())) |>
                  fit(mod)
  mod <- set_n_draw(mod, 5)
  comp <- make_comp_components(mod)
  term <- make_term_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(component = comp,
                               term = term,
                               level = level,
                               .fitted = .fitted)
  matrix_along_by <- choose_matrices_along_by(mod)[["sex:time"]]
  comp_reformatted <- reformat_hyperrand_one(prior = mod$priors[["sex:time"]],
                                             nm_prior <- "sex:time",
                                             matrix_along_by = matrix_along_by,
                                             components = components)
  ans_obtained <- subset(comp_reformatted, component == "effect" & term == "sex:time")
  ans_expected <- subset(comp_reformatted, component %in% c("trend", "cyclical", "error"))
  ans_expected <- aggregate(ans_expected[".fitted"], ans_expected["level"], sum)
  ans_expected <- ans_expected[match(ans_obtained$level, ans_expected$level),]
  ans_expected <- ans_expected$.fitted
  ans_obtained <- ans_obtained$.fitted
  expect_equal(ans_obtained, ans_expected)
})

test_that("'reformat_hyperrand_one' works with bage_prior_elin", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ ELin()) |>
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


## 'str_call_prior' -----------------------------------------------------------

test_that("'str_call_prior' works with bage_prior_ar - AR1", {
    expect_identical(str_call_prior(AR1()), "AR1()")
    expect_identical(str_call_prior(AR1(min = 0.5)), "AR1(min=0.5)")
    expect_identical(str_call_prior(AR1(max = 0.95)), "AR1(max=0.95)")
    expect_identical(str_call_prior(AR1(s = 0.3)), "AR1(s=0.3)")
    expect_identical(str_call_prior(AR1(min = 0.5, max = 0.95, s = 0.3)),
                     "AR1(min=0.5,max=0.95,s=0.3)")
})

test_that("'str_call_prior' works with bage_prior_ar - AR", {
    expect_identical(str_call_prior(AR(n = 1)), "AR(n=1)")
    expect_identical(str_call_prior(AR(n = 3, s = 0.3)), "AR(n=3,s=0.3)")
    expect_identical(str_call_prior(AR(s = 0.3, n = 2)),
                     "AR(s=0.3)")
})

test_that("'str_call_prior' works with bage_prior_compose", {
  expect_identical(str_call_prior(compose_time(Lin(), error = N())),
                   "compose_time(trend=Lin(), error=N())")
  expect_identical(str_call_prior(compose_time(trend = Lin(), seasonal = Seas(n = 3))),
                   "compose_time(trend=Lin(), seasonal=Seas(n=3))")
  expect_identical(str_call_prior(compose_time(error = N(s = 0.1), trend = ELin())),
                   "compose_time(trend=ELin(), error=N(s=0.1))")
})

test_that("'str_call_prior' works with bage_prior_ear - EAR1", {
    expect_identical(str_call_prior(EAR1()), "EAR1()")
    expect_identical(str_call_prior(EAR1(min = 0.5)), "EAR1(min=0.5)")
    expect_identical(str_call_prior(EAR1(max = 0.95)), "EAR1(max=0.95)")
    expect_identical(str_call_prior(EAR1(s = 0.3)), "EAR1(s=0.3)")
    expect_identical(str_call_prior(EAR1(min = 0.5, max = 0.95, s = 0.3)),
                     "EAR1(min=0.5,max=0.95,s=0.3)")
})

test_that("'str_call_prior' works with bage_prior_ear - EAR", {
    expect_identical(str_call_prior(EAR(n = 1)), "EAR(n=1)")
    expect_identical(str_call_prior(EAR(n = 3, s = 0.3)), "EAR(n=3,s=0.3)")
    expect_identical(str_call_prior(EAR(s = 0.3, n = 2)),
                     "EAR(n=2,s=0.3)")
})

test_that("'str_call_prior' works with bage_prior_elin", {
    expect_identical(str_call_prior(ELin()), "ELin()")
    expect_identical(str_call_prior(ELin(sd = 0.5, s = 2, ms = 1.2, along = "a")),
                     "ELin(s=2,sd=0.5,ms=1.2,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_erw", {
    expect_identical(str_call_prior(ERW()), "ERW()")
    expect_identical(str_call_prior(ERW(along = "a", s = 2)),
                     "ERW(s=2,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_eseas", {
    expect_identical(str_call_prior(ESeas(n = 4)), "ESeas(n=4)")
    expect_identical(str_call_prior(ESeas(s = 2, along = "a", n = 2)),
                     "ESeas(n=2,s=2,along=\"a\")")
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
    expect_identical(str_call_prior(Lin(sd = 0.5)), "Lin(sd=0.5)")
    expect_identical(str_call_prior(Lin(s = 0.95)), "Lin(s=0.95)")
    expect_identical(str_call_prior(Lin(sd = 0.1, s = 0.95)), "Lin(s=0.95,sd=0.1)")
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
    expect_identical(str_call_prior(RW(s = 0.95)), "RW(s=0.95)")
})

test_that("'str_call_prior' works with bage_prior_rw2", {
    expect_identical(str_call_prior(RW2()), "RW2()")
    expect_identical(str_call_prior(RW2(sd = 0.5)), "RW2(sd=0.5)")
    expect_identical(str_call_prior(RW2(s = 0.95)), "RW2(s=0.95)")
    expect_identical(str_call_prior(RW2(sd = 0.1, s = 0.95)), "RW2(s=0.95,sd=0.1)")
})

test_that("'str_call_prior' works with bage_prior_seas", {
    expect_identical(str_call_prior(Seas(n = 2)), "Seas(n=2)")
    expect_identical(str_call_prior(Seas(s = 3.2, n = 2)), "Seas(n=2,s=3.2)")
})

test_that("'str_call_prior' works with bage_prior_spline", {
    expect_identical(str_call_prior(Sp()), "Sp()")
    expect_identical(str_call_prior(Sp(n = 5L)), "Sp(n=5)")
    expect_identical(str_call_prior(Sp(s = 0.1)), "Sp(s=0.1)")
    expect_identical(str_call_prior(Sp(s = 3,n = 5L)), "Sp(n=5,s=3)")
})

test_that("'str_call_prior' works with bage_prior_svd", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(SVD(s)), "SVD(s)")
    expect_identical(str_call_prior(SVDS(s,joint = TRUE)), "SVDS(s,joint=TRUE)")
    expect_identical(str_call_prior(SVD(s,n = 6L)), "SVD(s,n=6)")
    expect_identical(str_call_prior(SVDS(s,joint=F,n = 3L)), "SVDS(s,n=3)")
})

test_that("'str_call_prior' works with bage_prior_esvd", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(ESVD(s)), "ESVD(s)")
    expect_identical(str_call_prior(ESVDS(s,joint = TRUE)), "ESVDS(s,joint=TRUE)")
    expect_identical(str_call_prior(ESVD(s,n = 6L)), "ESVD(s,n=6)")
    expect_identical(str_call_prior(ESVDS(s,joint=F,n = 3L)), "ESVDS(s,n=3)")
})


## 'str_nm_prior' -----------------------------------------------------------

test_that("'str_nm_prior' works with bage_prior_ar - AR1", {
  expect_identical(str_nm_prior(AR1(s = 3)), "AR1()")
})

test_that("'str_nm_prior' works with bage_prior_ar - AR", {
   expect_identical(str_nm_prior(AR(n = 1)), "AR()")
   expect_identical(str_nm_prior(AR(n = 3, s = 0.3)), "AR()")
})

test_that("'str_nm_prior' works with bage_prior_compose", {
  expect_identical(str_nm_prior(compose_time(Lin(), error = N())),
                   "compose_time()")
})

test_that("'str_nm_prior' works with bage_prior_ear - EAR1", {
    expect_identical(str_nm_prior(EAR1()), "EAR1()")
    expect_identical(str_nm_prior(EAR1(min = 0.5)), "EAR1()")
})

test_that("'str_nm_prior' works with bage_prior_ear - EAR", {
    expect_identical(str_nm_prior(EAR(n = 3, s = 0.3)), "EAR()")
})

test_that("'str_nm_prior' works with bage_prior_elin", {
    expect_identical(str_nm_prior(ELin(sd = 0.5, s = 2, ms = 1.2, along = "a")),
                     "ELin()")
})

test_that("'str_nm_prior' works with bage_prior_erw", {
    expect_identical(str_nm_prior(ERW()), "ERW()")
    expect_identical(str_nm_prior(ERW(along = "a", s = 2)), "ERW()")
})

test_that("'str_nm_prior' works with bage_prior_eseas", {
    expect_identical(str_nm_prior(ESeas(n = 4)), "ESeas()")
    expect_identical(str_nm_prior(ESeas(s = 2, along = "a", n = 2)), "ESeas()")
})

test_that("'str_nm_prior' works with bage_prior_known", {
    expect_identical(str_nm_prior(Known(1)), "Known()")
    expect_identical(str_nm_prior(Known(c(2, 3, -2, 0,7, 3))), "Known()")
})

test_that("'str_nm_prior' works with bage_prior_lin", {
    expect_identical(str_nm_prior(Lin()), "Lin()")
    expect_identical(str_nm_prior(Lin(sd = 0.1, s = 0.95)), "Lin()")
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

test_that("'str_nm_prior' works with bage_prior_rw2", {
    expect_identical(str_nm_prior(RW2()), "RW2()")
    expect_identical(str_nm_prior(RW2(sd = 0.1, s = 0.95)), "RW2()")
})

test_that("'str_nm_prior' works with bage_prior_seas", {
    expect_identical(str_nm_prior(Seas(n = 2)), "Seas()")
    expect_identical(str_nm_prior(Seas(s = 3.2, n = 2)), "Seas()")
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

test_that("'transform_hyper' works with 'bage_prior_compose'", {
  prior <- compose_time(trend = Lin(), cyclical = AR())
  ans_obtained <- transform_hyper(prior = prior)
  ans_expected <- unlist(c(list(trend = transform_hyper(Lin())),
                           list(cyclical = transform_hyper(AR()))))
  expect_equal(ans_obtained, ans_expected, ignore_function_env = TRUE)
})

test_that("'transform_hyper' works with 'bage_prior_ar - EAR1'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper(prior = EAR1())
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_ar - EAR'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper(prior = EAR(n = 2))
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_elin'", {
  l <- transform_hyper(prior = ELin())
  expect_equal(0.35, l[[1]](0.35))
  expect_equal(exp(0.35), l[[2]](0.35))
  expect_equal(exp(0.35), l[[3]](0.35))
})

test_that("'transform_hyper' works with 'bage_prior_erw'", {
  l <- transform_hyper(prior = ERW())
  expect_equal(length(l), 1L)
  expect_equal(exp(0.35), l[[1]](0.35))
})

test_that("'transform_hyper' works with 'bage_prior_eseas'", {
  l <- transform_hyper(prior = ESeas(n = 4))
  expect_equal(length(l), 1L)
  expect_equal(exp(0.35), l[[1]](0.35))
})

test_that("'transform_hyper' works with 'bage_prior_known'", {
  l <- transform_hyper(prior = Known(1))
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_lin'", {
  l <- transform_hyper(prior = Lin())
  expect_equal(0.35, l[[1]](0.35))
  expect_equal(exp(0.35), l[[2]](0.35))
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

test_that("'transform_hyper' works with 'bage_prior_rw2'", {
  l <- transform_hyper(prior = RW2())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_seas'", {
  l <- transform_hyper(prior = Seas(n = 4))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_spline'", {
  l <- transform_hyper(prior = Sp())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_svd'", {
  l <- transform_hyper(prior = SVD(HMD))
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_svd'", {
  l <- transform_hyper(prior = ESVD(HMD))
  expect_identical(l, list())
})


## 'transform_hyperrand' ------------------------------------------------------

test_that("'transform_hyperrand' works with 'bage_prior_ar1'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  prior <- AR1()
  ans_obtained <- transform_hyperrand(prior = prior, matrix_along_by = matrix_along_by)
  ans_expected <- list()
  expect_equal(ans_obtained, ans_expected)
})

test_that("'transform_hyperrand' works with 'bage_prior_compose'", {
  matrix_along_by <- matrix(0:9, nc = 2)
  prior <- compose_time(trend = ELin(), cyclical = EAR(), error = N())
  ans_obtained <- transform_hyperrand(prior = prior, matrix_along_by = matrix_along_by)
  ans_expected <- c(rep(list(trend.effect = identity), times = 10),
                    rep(list(trend.mslope = identity), times = 2),
                    rep(list(cyclical.effect = identity), times = 10))
  expect_equal(ans_obtained, ans_expected, ignore_function_env = TRUE)
})

test_that("'transform_hyperrand' works with 'bage_prior_elin'", {
  matrix_along_by <- matrix(0:9, nc = 2)
  l <- transform_hyperrand(prior = ELin(),
                           matrix_along_by = matrix_along_by)
  expect_equal(0.35, l[[1]](0.35))
  expect_equal(0.35, l[[2]](0.35))
  expect_identical(length(l), 2L)
})


## use_for_compose_cyclical ---------------------------------------------------

test_that("'use_for_compose_cyclical' returns TRUE with priors that can be used for cyclical", {
  expect_true(use_for_compose_cyclical(AR1()))
  expect_true(use_for_compose_cyclical(EAR()))
})

test_that("'use_for_compose_cyclical' returns FALSE with priors that cannot be used for cyclical", {
  expect_false(use_for_compose_cyclical(ESeas(n = 12)))
  expect_false(use_for_compose_cyclical(ELin()))
  expect_false(use_for_compose_cyclical(ERW()))
  expect_false(use_for_compose_cyclical(Known(c(a = 1, b = -1))))
  expect_false(use_for_compose_cyclical(Lin()))
  expect_false(use_for_compose_cyclical(NFix()))
  expect_false(use_for_compose_cyclical(RW()))
  expect_false(use_for_compose_cyclical(RW2()))
  expect_false(use_for_compose_cyclical(Seas(n = 4)))
  expect_false(use_for_compose_cyclical(Sp()))
  expect_false(use_for_compose_cyclical(SVD(HMD)))
})


## use_for_compose_error ------------------------------------------------------

test_that("'use_for_compose_error' returns TRUE with priors that can be used for error", {
  expect_true(use_for_compose_error(N()))
})

test_that("'use_for_compose_error' returns FALSE with priors that cannot be used for error", {
  expect_false(use_for_compose_error(AR1()))
  expect_false(use_for_compose_error(EAR1()))
  expect_false(use_for_compose_error(ELin()))
  expect_false(use_for_compose_error(ERW()))
  expect_false(use_for_compose_error(ESeas(n = 12)))
  expect_false(use_for_compose_error(Known(c(a = 1, b = -1))))
  expect_false(use_for_compose_error(Lin()))
  expect_false(use_for_compose_error(NFix()))
  expect_false(use_for_compose_error(RW()))
  expect_false(use_for_compose_error(RW2()))
  expect_false(use_for_compose_error(Seas(n = 4)))
  expect_false(use_for_compose_error(Sp()))
  expect_false(use_for_compose_error(SVD(HMD)))
})


## use_for_compose_seasonal ------------------------------------------------------

test_that("'use_for_compose_seasonal' returns TRUE with priors that can be used for seasonal", {
    expect_true(use_for_compose_seasonal(ESeas(n = 12)))
    expect_true(use_for_compose_seasonal(Seas(n = 4)))
})

test_that("'use_for_compose_seasonal' returns FALSE with priors that cannot be used for seasonal", {
    expect_false(use_for_compose_seasonal(AR1()))
    expect_false(use_for_compose_seasonal(EAR1()))
    expect_false(use_for_compose_seasonal(ELin()))
    expect_false(use_for_compose_seasonal(ERW()))
    expect_false(use_for_compose_seasonal(Lin()))
    expect_false(use_for_compose_seasonal(N()))
    expect_false(use_for_compose_seasonal(NFix()))
    expect_false(use_for_compose_seasonal(RW()))
    expect_false(use_for_compose_seasonal(RW2()))
    expect_false(use_for_compose_seasonal(Sp()))
    expect_false(use_for_compose_seasonal(Known(c(a = 1, b = -1))))
    expect_false(use_for_compose_seasonal(SVD(HMD)))
})


## use_for_compose_trend ------------------------------------------------------

test_that("'use_for_compose_trend' returns TRUE with priors that can be used for trend", {
    expect_true(use_for_compose_trend(ELin()))
    expect_true(use_for_compose_trend(ERW()))
    expect_true(use_for_compose_trend(Lin()))
    expect_true(use_for_compose_trend(RW()))
    expect_true(use_for_compose_trend(RW2()))
    expect_true(use_for_compose_trend(Sp()))
})

test_that("'use_for_compose_trend' returns FALSE with priors that cannot be used for trend", {
    expect_false(use_for_compose_trend(AR1()))
    expect_false(use_for_compose_trend(EAR1()))
    expect_false(use_for_compose_trend(Known(c(a = 1, b = -1))))
    expect_false(use_for_compose_trend(N()))
    expect_false(use_for_compose_trend(NFix()))
    expect_false(use_for_compose_trend(Seas(n = 4)))
    expect_false(use_for_compose_trend(ESeas(n = 12)))
    expect_false(use_for_compose_trend(SVD(HMD)))
})


## use_for_interaction --------------------------------------------------------

test_that("'use_for_interaction' returns FALSE with priors that are not necessarily interactions", {
    expect_false(use_for_interaction(AR1()))
    expect_false(use_for_interaction(Lin()))
    expect_false(use_for_interaction(RW()))
    expect_false(use_for_interaction(RW2()))
    expect_false(use_for_interaction(Sp()))
    expect_false(use_for_interaction(N()))
    expect_false(use_for_interaction(NFix()))
    expect_false(use_for_interaction(Seas(n = 3)))
    expect_false(use_for_interaction(SVD(HMD)))
})

test_that("'use_for_interaction' returns TRUE with priors that are always interactions", {
    expect_true(use_for_interaction(EAR()))
    expect_true(use_for_interaction(ELin()))
    expect_true(use_for_interaction(ERW()))
    expect_true(use_for_interaction(ESeas(n = 12)))
    expect_true(use_for_interaction(ESVD(HMD)))
})


## use_for_main_effect --------------------------------------------------------

test_that("'use_for_main_effect' returns FALSE with priors that are not necessarily main effects", {
    expect_false(use_for_main_effect(EAR1()))
    expect_false(use_for_main_effect(ELin()))
    expect_false(use_for_main_effect(ERW()))
    expect_false(use_for_main_effect(ESeas(n = 12)))
    expect_false(use_for_main_effect(N()))
    expect_false(use_for_main_effect(NFix()))
    expect_false(use_for_main_effect(Seas(n = 3)))
    expect_false(use_for_main_effect(SVD(HMD)))
    expect_false(use_for_main_effect(ESVD(HMD)))
})

test_that("'use_for_main_effect' returns TRUE with priors that are always main effects", {
    expect_true(use_for_main_effect(AR1()))
    expect_true(use_for_main_effect(Lin()))
    expect_true(use_for_main_effect(RW()))
    expect_true(use_for_main_effect(RW2()))
    expect_true(use_for_main_effect(Sp()))
})


## uses_along -----------------------------------------------------------------

test_that("'uses_along' works with valid inputs", {
    expect_false(uses_along(N()))
    expect_true(uses_along(EAR()))
    expect_true(uses_along(ELin()))
    expect_true(uses_along(ERW()))
    expect_true(uses_along(ESeas(n = 2)))
})

test_that("'uses_along' works with 'compose_time", {
  prior <- compose_time(trend = RW(), error = N())
  expect_false(uses_along(prior))
  prior <- compose_time(trend = ERW(), error = N())
  expect_true(uses_along(prior))
})


## uses_hyperrand ------------------------------------------------------

test_that("'uses_hyperrand' returns TRUE with priors that can be used for seasonal", {
  expect_true(uses_hyperrand(compose_time(ERW(), err = N())))
  expect_true(uses_hyperrand(ELin()))
})

test_that("'uses_hyperrand' returns FALSE with priors that cannot be used for seasonal", {
  expect_false(uses_hyperrand(AR1()))
  expect_false(uses_hyperrand(EAR1()))
  expect_false(uses_hyperrand(ERW()))
  expect_false(uses_hyperrand(ESeas(n = 12)))
  expect_false(uses_hyperrand(Known(c(a = 1, b = -1))))
  expect_false(uses_hyperrand(Lin()))
  expect_false(uses_hyperrand(N()))
  expect_false(uses_hyperrand(NFix()))
  expect_false(uses_hyperrand(RW()))
  expect_false(uses_hyperrand(RW2()))
  expect_false(uses_hyperrand(Seas(n = 4)))
  expect_false(uses_hyperrand(Sp()))
  expect_false(uses_hyperrand(SVD(HMD)))
  expect_false(uses_hyperrand(ESVD(HMD)))
})


## uses_matrix_effectfree_effect ----------------------------------------------

test_that("'uses_matrix_effectfree_effect' works with valid inputs", {
    expect_false(uses_matrix_effectfree_effect(N()))
    expect_true(uses_matrix_effectfree_effect(Sp()))
    expect_true(uses_matrix_effectfree_effect(SVD(HMD)))
    expect_true(uses_matrix_effectfree_effect(ESVD(HMD)))
})


## uses_offset_effectfree_effect ----------------------------------------------

test_that("'uses_offset_effectfree_effect' works with valid inputs", {
    expect_false(uses_offset_effectfree_effect(N()))
    expect_false(uses_offset_effectfree_effect(Sp()))
    expect_true(uses_offset_effectfree_effect(SVD(HMD)))
    expect_true(uses_offset_effectfree_effect(ESVD(HMD)))
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

test_that("'vals_hyper_to_dataframe' works with bage_prior_compose", {
  prior <- compose_time(RW2(), cyclical = AR(), seasonal = Seas(n = 2))
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = 10)
  ans_obtained <- vals_hyper_to_dataframe(prior = prior,
                                          nm_prior = "time",
                                          vals_hyper = vals_hyper,
                                          n_sim = 10)
  fitted <- vctrs::vec_rbind(vals_hyper[[1]][[1]],
                             vals_hyper[[2]][[1]],
                             vals_hyper[[2]][[2]],
                             vals_hyper[[3]][[1]],
                             .name_repair = "universal_quiet")
  fitted <- as.matrix(fitted)
  dimnames(fitted) <- NULL
  ans_expected <- tibble::tibble(term = "time",
                                 component = "hyper",
                                 level = c("trend.sd",
                                           "cyclical.coef1", "cyclical.coef2", "cyclical.sd",
                                           "seasonal.sd"),
                                 .fitted = rvec::rvec(fitted))
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


## values_known ---------------------------------------------------------------

test_that("'values_known' works with valid inputs", {
    expect_identical(values_known(Known(values = 1:3)),
                     as.double(1:3))
})
