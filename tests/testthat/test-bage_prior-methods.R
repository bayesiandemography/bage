
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
                          agesex = NULL,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_iar", {
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
                          agesex = NULL,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_ilin", {
  prior <- ILin(s = 0.01)
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
                          agesex = NULL,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters[1:12], as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_iseas", {
  prior <- ISeas(n = 2, s = 0.01)
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
                          agesex = NULL,
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
                          agesex = NULL,
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
                          agesex = NULL,
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
                          agesex = NULL,
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
                          agesex = NULL,
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
                          agesex = NULL,
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
                          agesex = NULL,
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
                          agesex = NULL,
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
                          agesex = NULL,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_svd", {
  prior <- SVD(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_effect <- c(0:79, "80+")
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          levels_effect = levels_effect,
                          agesex = "age",
                          matrix_along_by = matrix_along_by,
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

test_that("'draw_vals_hyper' works with bage_prior_iar", {
  prior <- IAR(n = 3)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("coef", "sd"))
  expect_identical(dim(ans$coef), c(3L, 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_ilin", {
  set.seed(0)
  prior <- ILin()
  n_sim <- 10
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = n_sim)
  expect_identical(names(ans), c("slope", "sd", "msd"))
  expect_identical(lengths(ans),
                   c(slope = 10L, sd = 10L, msd = 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_iseas", {
  set.seed(0)
  prior <- ISeas(n = 2)
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

test_that("'draw_vals_hyperrand' works with bage_prior_ilin", {
  set.seed(0)
  prior <- ILin()
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             matrix_along_by = matrix_along_by,
                             n_sim = n_sim)
  expect_identical(names(ans), "mslope")
  expect_identical(lengths(ans),
                   c(mslope = 40L))
})

test_that("'draw_vals_hyperrand' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  matrix_along_by <- matrix(0:11, nr = 12)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             matrix_along_by = matrix_along_by,
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
  prior <- ILin()
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

test_that("'indices_priors' works with compose time prior - 1 prior", {
  prior <- compose_time(trend = RW2())
  matrix_along_by <- matrix(0:9, nr = 10)
  ans_obtained <- indices_priors(prior = prior,
                                 matrix_along_by = matrix_along_by)
  ans_expected <- c(hyper_start = 0L,
                    hyper_length = 1L,
                    hyperrand_start = 0L,
                    hyperrand_length = 0L,
                    consts_start = 0L,
                    consts_length = 2L,
                    i_prior = 4L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'indices_priors' works with compose time prior - 2 priors", {
  prior <- compose_time(trend = RW2(),
                        cyclical = AR())
  matrix_along_by <- matrix(0:9, nr = 10)
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
                    hyperrand_length = 0L,
                    consts_start = 2L,
                    consts_length = 5L,
                    i_prior = 5L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'indices_priors' works with compose time prior - 3 priors", {
  prior <- compose_time(trend = RW2(),
                        cyclical = AR(),
                        error = N())
  matrix_along_by <- matrix(0:9, nr = 10)
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


## is_comparable_prior --------------------------------------------------------

test_that("'is_comparable_prior' returns FALSE when priors differ", {
    expect_false(is_comparable_prior(AR1(), N()))
})

test_that("'is_comparable_prior' returns TRUE when priors same (and not specific methods)", {
    expect_true(is_comparable_prior(AR1(), AR1(s = 3)))
})

test_that("'is_comparable_prior' returns TRUE when priors same (and not specific methods)", {
    expect_true(is_comparable_prior(AR1(), AR1(s = 3)))
})

test_that("'is_comparable_prior' works with RW2)", {
    expect_true(is_comparable_prior(RW2(), RW2(s = 3)))
    expect_false(is_comparable_prior(RW2(sd = 0.3), RW2()))
})

test_that("'is_comparable_prior' works with spline)", {
    expect_true(is_comparable_prior(Sp(), Sp(s = 3)))
    expect_false(is_comparable_prior(Sp(n = 4), Sp(n = 5)))
})

test_that("'is_comparable_prior' works with SVD)", {
    expect_true(is_comparable_prior(SVD(HMD), SVD(HMD)))
    expect_false(is_comparable_prior(SVD(HMD, n = 4), SVD(HMD, n = 5)))
    expect_false(is_comparable_prior(SVD(HMD, indep = TRUE), SVD(HMD, indep = FALSE)))
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
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_iar", {
    expect_true(is_prior_ok_for_term(prior = IAR(n = 3),
                                     nm = "time:region",
                                     matrix_along_by = matrix(0:29, nc = 3),
                                     var_time = "time",
                                     var_age = "age",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_ilin", {
    expect_true(is_prior_ok_for_term(prior = ILin(),
                                     nm = "sex:time",
                                     matrix_along_by = matrix(0:11, nc = 2),
                                     var_time = "time",
                                     var_age = "age",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_iseas", {
  expect_true(is_prior_ok_for_term(prior = ISeas(n = 4),
                                   nm = "time",
                                   matrix_along_by = matrix(0:12, nc = 3),
                                   var_time = "time",
                                   var_age = "age",
                                   is_in_compose = TRUE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_iseas", {
  expect_error(is_prior_ok_for_term(prior = ISeas(n = 4),
                                    nm = "time",
                                    matrix_along_by = matrix(0:12, nc = 3),
                                    var_time = "time",
                                    var_age = "age",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "`ISeas\\(n=4\\)` prior cannot be used on its own.")
})

test_that("'is_prior_ok_for_term' works with bage_prior_known", {
  expect_true(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                   nm = "sex",
                                   matrix_along_by = matrix(0:1, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_lin", {
  expect_true(is_prior_ok_for_term(prior = Lin(),
                                   nm = "sex",
                                   matrix_along_by = matrix(0:1, nc = 1),
                                   var_time = "time",
                                   var_age = "age",
                                   is_in_compose = FALSE,
                                   agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws expected error with bage_prior_known", {
  expect_error(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                    nm = "sex",
                                    matrix_along_by = matrix(0:2, nc = 1),
                                    var_time = "time",
                                    var_age = "age",
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
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_normfixed", {
    expect_true(is_prior_ok_for_term(prior = NFix(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw", {
    expect_true(is_prior_ok_for_term(prior = RW(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
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
                                     is_in_compose = FALSE,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd, correct inputs", {
    expect_true(is_prior_ok_for_term(prior = SVD(sim_scaled_svd()),
                                     nm = "age:sex",
                                     matrix_along_by = matrix(0:9, nc = 2),
                                     var_time = "time",
                                     var_age = "age",
                                     is_in_compose = FALSE,
                                     agesex = "age:sex"))
})

test_that("'is_prior_ok_for_term' throws correct error with main effect, agesex NULL", {
    s <- sim_scaled_svd()
    expect_error(is_prior_ok_for_term(prior = SVD(s),
                                     nm = "bla",
                                     matrix_along_by = matrix(0:9, nc = 1),
                                     var_time = "time",
                                     var_age = "age",
                                     is_in_compose = FALSE,
                                     agesex = NULL),
                "Problem with `SVD\\(s\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' throws correct error with interaction, agesex NULL", {
    s <- sim_scaled_svd()
    expect_error(is_prior_ok_for_term(prior = SVD(s),
                                     nm = "bla:bleh",
                                     matrix_along_by = matrix(0:9, nc = 2),
                                     var_time = "time",
                                     var_age = "age",
                                     is_in_compose = FALSE,
                                     agesex = NULL),
                "Problem with `SVD\\(s\\)` prior for `bla:bleh` term.")
})

test_that("'is_prior_ok_for_term' throws correct error order-3 interaction, agesex 'other'", {
  s <- sim_scaled_svd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "bla:bleh:blu",
                                    matrix_along_by = matrix(0:11, nc = 2),
                                    var_time = "time",
                                    var_age = "age",
                                    is_in_compose = FALSE,
                                    agesex = "other"),
               "Problem with `SVD\\(s\\)` prior for `bla:bleh:blu` term.")
})


## levels_hyper ---------------------------------------------------------------

test_that("'levels_hyper' works with 'bage_prior_ar'", {
  expect_identical(levels_hyper(prior = AR(n = 2)),
                   c("coef", "coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_compose - time'", {
  prior <- compose_time(trend = Lin(), cyclical = AR(), seasonal = Seas(n = 4))
  expect_identical(levels_hyper(prior),
                   c(trend = levels_hyper(Lin()),
                     cyclical = levels_hyper(AR()),
                     seasonal = levels_hyper(Seas(n = 4))))
})

test_that("'levels_hyper' works with 'bage_prior_iar'", {
  expect_identical(levels_hyper(prior = IAR(n = 2)),
                   c("coef", "coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_ilin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = ILin()),
                   c("slope", "sd", "msd"))
})

test_that("'levels_hyper' works with 'bage_prior_iseas'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = ISeas(n = 3)),
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
  expect_identical(levels_hyper(prior = SVD(sim_scaled_svd())),
                   character())
})


## levels_hyperrand ---------------------------------------------------------------

test_that("'levels_hyperrand' works with 'bage_prior_ar'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyperrand(prior = AR(n = 2), matrix_along_by = matrix_along_by),
                   character())
})

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 1 prior", {
  prior <- compose_time(ILin())
  matrix_along_by <- matrix(0:99, ncol = 5)
  expect_identical(levels_hyperrand(prior = prior, matrix_along_by = matrix_along_by),
                   rep("mslope", 5))
})

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 2 priors", {
  prior <- compose_time(ILin(), seasonal = ISeas(n = 4))
  matrix_along_by <- matrix(0:99, ncol = 5)
  expect_identical(levels_hyperrand(prior = prior, matrix_along_by = matrix_along_by),
                   c(rep("effect", 100), rep("mslope", 5)))
})

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 3 priors", {
  prior <- compose_time(ILin(), seasonal = ISeas(n = 4), cyclical = IAR())
  matrix_along_by <- matrix(0:99, ncol = 5)
  expect_identical(levels_hyperrand(prior = prior, matrix_along_by = matrix_along_by),
                   c(rep("effect", 100),
                     rep("mslope", 5),
                     rep("effect", 100)))
})

test_that("'levels_hyperrand' works with 'bage_prior_compose' - 4 priors", {
  prior <- compose_time(ILin(), seasonal = ISeas(n = 4), cyclical = IAR(), error = N())
  matrix_along_by <- matrix(0:99, ncol = 5)
  expect_identical(levels_hyperrand(prior = prior, matrix_along_by = matrix_along_by),
                   c(rep("effect", 100),
                     rep("mslope", 5),
                     rep("effect", 200)))
})

test_that("'levels_hyperrand' works with 'bage_prior_ilin'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyperrand(prior = ILin(), matrix_along_by = matrix_along_by),
                   c("mslope", "mslope"))
})

## 'make_matrix_effectfree_effect' --------------------------------------------------

test_that("'make_matrix_effectfree_effect' works with bage_prior_ar1", {
  prior <- AR1()
  levels_effect <- 2001:2005
  agesex <- "other"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                levels_effect = levels_effect,
                                                agesex = agesex)
  ans_expected <- Matrix::.sparseDiagonal(5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - n supplied", {
    prior <- Sp(n = 5)
    levels_effect <- 1:10
    ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                            levels_effect = levels_effect,
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
                                            agesex = agesex)
    ans_expected <- make_spline_matrix(length_effect = 10,
                                       n_spline = 7)
    rownames(ans_expected) <- levels_effect
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age main effect", {
    s <- sim_scaled_svd()
    prior <- SVD(scaled_svd = s, n = 3)
    levels_effect <- c("0-4", "5-9")
    agesex <- "age"
    ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                            levels_effect = levels_effect,
                                            agesex = agesex)
    ans_expected <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
    ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                         j = col(ans_expected),
                                         x = as.double(ans_expected),
                                         dimnames = dimnames(ans_expected))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age-sex interaction, joint", {
    s <- sim_scaled_svd()
    prior <- SVD(scaled_svd = s, n = 3, indep = FALSE)
    levels_effect <- c("Female.0-4", "Female.5-9", "Male.0-4", "Male.5-9")
    agesex <- "sex:age"
    ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                            levels_effect = levels_effect,
                                            agesex = agesex)
    ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][,1:3]
    ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                         j = col(ans_expected),
                                         x = as.double(ans_expected),
                                         dimnames = dimnames(ans_expected))
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
    s <- sim_scaled_svd()
    prior <- SVD(scaled_svd = s, n = 3)
    levels_effect <- c("0-4", "5-9")
    agesex <- "age"
    ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                            levels_effect = levels_effect,
                                            agesex = agesex)
    ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd - age-sex interaction, joint", {
    s <- sim_scaled_svd()
    prior <- SVD(scaled_svd = s, n = 3, indep = FALSE)
    levels_effect <- c("Female.0-4", "Female.5-9", "Male.0-4", "Male.5-9")
    agesex <- "sex:age"
    ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                            levels_effect = levels_effect,
                                            agesex = agesex)
    ans_expected <- s$data$offset[s$data$type == "joint"][[1L]]
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
  expect_identical(str_call_prior(compose_time(Lin())),
                   "compose_time(trend=Lin())")
  expect_identical(str_call_prior(compose_time(trend = Lin(), seasonal = Seas(n = 3))),
                   "compose_time(trend=Lin(), seasonal=Seas(n=3))")
  expect_identical(str_call_prior(compose_time(error = N(s = 0.1), trend = ILin())),
                   "compose_time(trend=ILin(), error=N(s=0.1))")
})

test_that("'str_call_prior' works with bage_prior_iar - IAR1", {
    expect_identical(str_call_prior(IAR1()), "IAR1()")
    expect_identical(str_call_prior(IAR1(min = 0.5)), "IAR1(min=0.5)")
    expect_identical(str_call_prior(IAR1(max = 0.95)), "IAR1(max=0.95)")
    expect_identical(str_call_prior(IAR1(s = 0.3)), "IAR1(s=0.3)")
    expect_identical(str_call_prior(IAR1(min = 0.5, max = 0.95, s = 0.3)),
                     "IAR1(min=0.5,max=0.95,s=0.3)")
})

test_that("'str_call_prior' works with bage_prior_iar - IAR", {
    expect_identical(str_call_prior(IAR(n = 1)), "IAR(n=1)")
    expect_identical(str_call_prior(IAR(n = 3, s = 0.3)), "IAR(n=3,s=0.3)")
    expect_identical(str_call_prior(IAR(s = 0.3, n = 2)),
                     "IAR(n=2,s=0.3)")
})

test_that("'str_call_prior' works with bage_prior_ilin", {
    expect_identical(str_call_prior(ILin()), "ILin()")
    expect_identical(str_call_prior(ILin(sd = 0.5, s = 2, ms = 1.2, along = "a")),
                     "ILin(s=2,sd=0.5,ms=1.2,along=\"a\")")
})

test_that("'str_call_prior' works with bage_prior_iseas", {
    expect_identical(str_call_prior(ISeas(n = 4)), "ISeas(n=4)")
    expect_identical(str_call_prior(ISeas(s = 2, along = "a", n = 2)),
                     "ISeas(n=2,s=2,along=\"a\")")
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
    s <- sim_scaled_svd()
    expect_identical(str_call_prior(SVD(s)), "SVD(s)")
    expect_identical(str_call_prior(SVD(s,indep = FALSE)), "SVD(s,indep=FALSE)")
    expect_identical(str_call_prior(SVD(s,n = 6L)), "SVD(s,n=6)")
    expect_identical(str_call_prior(SVD(s,indep=F,n = 3L)), "SVD(s,n=3,indep=FALSE)")
})


## transform_hyper ------------------------------------------------------------

test_that("'transform_hyper' works with 'bage_prior_ar - AR1'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper(prior = AR1(), matrix_along_by = matrix_along_by)
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_ar - AR'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper(prior = AR(n = 2), matrix_along_by = matrix_along_by)
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_ar - IAR1'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper(prior = IAR1(), matrix_along_by = matrix_along_by)
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_ar - IAR'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper(prior = IAR(n = 2), matrix_along_by = matrix_along_by)
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_ilin'", {
  matrix_along_by <- matrix(0:9, nc = 2)
  l <- transform_hyper(prior = ILin(),
                       matrix_along_by = matrix_along_by)
  expect_equal(0.35, l[[1]](0.35))
  expect_equal(0.35, l[[2]](0.35))
  expect_equal(0.35, l[[3]](0.35))
  expect_equal(exp(0.35), l[[4]](0.35))
  expect_equal(exp(0.35), l[[5]](0.35))
})

test_that("'transform_hyper' works with 'bage_prior_known'", {
  matrix_along_by <- matrix(0, nc = 1)
  l <- transform_hyper(prior = Known(1), matrix_along_by = matrix_along_by)
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_lin'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  l <- transform_hyper(prior = Lin(), matrix_along_by = matrix_along_by)
  expect_equal(0.35, l[[1]](0.35))
  expect_equal(exp(0.35), l[[2]](0.35))
})

test_that("'transform_hyper' works with 'bage_prior_norm'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  l <- transform_hyper(prior = N(), matrix_along_by = matrix_along_by)
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_normfixed'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  l <- transform_hyper(prior = NFix(), matrix_along_by = matrix_along_by)
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_rw'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  l <- transform_hyper(prior = RW(), matrix_along_by = matrix_along_by)
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  l <- transform_hyper(prior = RW2(), matrix_along_by = matrix_along_by)
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_seas'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  l <- transform_hyper(prior = Seas(n = 4), matrix_along_by = matrix_along_by)
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_spline'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  l <- transform_hyper(prior = Sp(), matrix_along_by = matrix_along_by)
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_svd'", {
  matrix_along_by <- matrix(0:9, nc = 1)
  l <- transform_hyper(prior = SVD(HMD), matrix_along_by = matrix_along_by)
  expect_identical(l, list())
})


## use_for_compose_cyclical ------------------------------------------------------

test_that("'use_for_compose_cyclical' returns TRUE with priors that can be used for cyclical", {
  expect_true(use_for_compose_cyclical(AR1()))
  expect_true(use_for_compose_cyclical(IAR()))
})

test_that("'use_for_compose_cyclical' returns FALSE with priors that cannot be used for cyclical", {
  expect_false(use_for_compose_cyclical(Seas(n = 4)))
  expect_false(use_for_compose_cyclical(ISeas(n = 12)))
  expect_false(use_for_compose_cyclical(ILin()))
  expect_false(use_for_compose_cyclical(Lin()))
  expect_false(use_for_compose_cyclical(NFix()))
  expect_false(use_for_compose_cyclical(RW()))
  expect_false(use_for_compose_cyclical(RW2()))
  expect_false(use_for_compose_cyclical(Sp()))
  expect_false(use_for_compose_cyclical(Known(c(a = 1, b = -1))))
  expect_false(use_for_compose_cyclical(SVD(HMD)))
})


## use_for_compose_error ------------------------------------------------------

test_that("'use_for_compose_error' returns TRUE with priors that can be used for error", {
  expect_true(use_for_compose_error(N()))
})

test_that("'use_for_compose_error' returns FALSE with priors that cannot be used for error", {
  expect_false(use_for_compose_error(Seas(n = 4)))
  expect_false(use_for_compose_error(ISeas(n = 12)))
  expect_false(use_for_compose_error(AR1()))
  expect_false(use_for_compose_error(IAR1()))
  expect_false(use_for_compose_error(ILin()))
  expect_false(use_for_compose_error(Lin()))
  expect_false(use_for_compose_error(NFix()))
  expect_false(use_for_compose_error(RW()))
  expect_false(use_for_compose_error(RW2()))
  expect_false(use_for_compose_error(Sp()))
  expect_false(use_for_compose_error(Known(c(a = 1, b = -1))))
  expect_false(use_for_compose_error(SVD(HMD)))
})


## use_for_compose_seasonal ------------------------------------------------------

test_that("'use_for_compose_seasonal' returns TRUE with priors that can be used for seasonal", {
    expect_true(use_for_compose_seasonal(Seas(n = 4)))
    expect_true(use_for_compose_seasonal(ISeas(n = 12)))
})

test_that("'use_for_compose_seasonal' returns FALSE with priors that cannot be used for seasonal", {
    expect_false(use_for_compose_seasonal(AR1()))
    expect_false(use_for_compose_seasonal(IAR1()))
    expect_false(use_for_compose_seasonal(ILin()))
    expect_false(use_for_compose_seasonal(Lin()))
    expect_false(use_for_compose_seasonal(N()))
    expect_false(use_for_compose_seasonal(NFix()))
    expect_false(use_for_compose_seasonal(RW()))
    expect_false(use_for_compose_seasonal(RW2()))
    expect_false(use_for_compose_seasonal(Sp()))
    expect_false(use_for_compose_trend(Known(c(a = 1, b = -1))))
    expect_false(use_for_compose_trend(SVD(HMD)))
})


## use_for_compose_trend ------------------------------------------------------

test_that("'use_for_compose_trend' returns TRUE with priors that can be used for trend", {
    expect_true(use_for_compose_trend(ILin()))
    expect_true(use_for_compose_trend(Lin()))
    expect_true(use_for_compose_trend(RW()))
    expect_true(use_for_compose_trend(RW2()))
    expect_true(use_for_compose_trend(Sp()))
})

test_that("'use_for_compose_trend' returns FALSE with priors that cannot be used for trend", {
    expect_false(use_for_compose_trend(AR1()))
    expect_false(use_for_compose_trend(IAR1()))
    expect_false(use_for_compose_trend(Known(c(a = 1, b = -1))))
    expect_false(use_for_compose_trend(N()))
    expect_false(use_for_compose_trend(NFix()))
    expect_false(use_for_compose_trend(Seas(n = 4)))
    expect_false(use_for_compose_trend(ISeas(n = 12)))
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
})

test_that("'use_for_interaction' returns TRUE with priors that are always interactions", {
    expect_true(use_for_interaction(IAR()))
    expect_true(use_for_interaction(ILin()))
    expect_true(use_for_interaction(ISeas(n = 12)))
})


## use_for_main_effect --------------------------------------------------------

test_that("'use_for_main_effect' returns FALSE with priors that are not necessarily main effects", {
    expect_false(use_for_main_effect(IAR1()))
    expect_false(use_for_main_effect(ILin()))
    expect_false(use_for_main_effect(ISeas(n = 12)))
    expect_false(use_for_main_effect(N()))
    expect_false(use_for_main_effect(NFix()))
    expect_false(use_for_main_effect(Seas(n = 3)))
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
    expect_true(uses_along(IAR()))
    expect_true(uses_along(ILin()))
    expect_true(uses_along(ISeas(n = 2)))
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


## values_known ---------------------------------------------------------------

test_that("'values_known' works with valid inputs", {
    expect_identical(values_known(Known(values = 1:3)),
                     as.double(1:3))
})

