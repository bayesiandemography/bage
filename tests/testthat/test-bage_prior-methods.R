
## draw_vals_effect --------------------------------------------------------------

test_that("'draw_vals_effect' works with bage_prior_ar", {
  prior <- AR(n = 3)
  n_sim <- 10
  matrix_along_by <- matrix(0:25, nc = 1)
  vals_hyper <- draw_vals_hyper(prior = prior,
                                matrix_along_by = matrix_along_by,                                
                                n_sim = n_sim)
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
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
                                matrix_along_by = matrix_along_by,
                                n_sim = n_sim)
  levels_effect <- letters[1:12]
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
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
  levels_effect <- c("a", "b", "c")
  matrix_along_by <- matrix(0:2, nc = 1)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = NULL,
                          levels_effect = levels_effect,
                          agesex = NULL,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(c("a", "b", "c"), as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_lin", {
  prior <- Lin()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                matrix_along_by = matrix_along_by,
                                n_sim = n_sim)
  matrix_along_by <- matrix(0:25, nc = 1)
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
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
  matrix_along_by <- matrix(0:999, nc = 10)
  vals_hyper <- draw_vals_hyper(prior = prior,
                                matrix_along_by = matrix_along_by,
                                n_sim = n_sim)
  levels_effect <- seq_len(1000)
  ans <- draw_vals_effect(prior = prior,
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
  levels_effect <- seq_len(1000)
  matrix_along_by <- matrix(0:999, nc = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = NULL,
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
  matrix_along_by <- matrix(0:25, nc = 1)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                matrix_along_by = matrix_along_by,
                                n_sim = n_sim)
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
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
  matrix_along_by <- matrix(0:25, nc = 1)
  vals_hyper <- draw_vals_hyper(prior = prior,
                                matrix_along_by = matrix_along_by,
                                n_sim = n_sim)
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          levels_effect = levels_effect,
                          agesex = NULL,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_spline", {
  prior <- Sp()
  matrix_along_by <- matrix(0:25, nc = 1)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                matrix_along_by = matrix_along_by,
                                n_sim = n_sim)
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          levels_effect = levels_effect,
                          agesex = NULL,
                          matrix_along_by = matrix_along_by,
                          n_sim = n_sim)
  expect_identical(dimnames(ans),
                   list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_svd", {
  prior <- SVD(HMD)
  matrix_along_by <- matrix(0:80, nc = 1)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                matrix_along_by = matrix_along_by,
                                n_sim = n_sim)
  levels_effect <- c(0:79, "80+")
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
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
  matrix_along_by <- matrix(0:9, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(names(ans), c("coef", "sd"))
  expect_identical(dim(ans$coef), c(3L, 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_ilin", {
  set.seed(0)
  prior <- ILin()
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = n_sim)
  expect_identical(names(ans), c("slope", "mslope", "sd", "msd"))
  expect_identical(lengths(ans),
                   c(slope = 10L, mslope = 40L, sd = 10L, msd = 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_known", {
  prior <- Known(c(0.1, 0.3))
  matrix_along_by <- matrix(0:1, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_lin", {
  prior <- Lin()
  matrix_along_by <- matrix(0:9, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(names(ans), c("slope", "sd"))
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_norm", {
  prior <- N()
  matrix_along_by <- matrix(0:9, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_normfixed", {
  prior <- NFix()
  matrix_along_by <- matrix(0:9, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_rw", {
  prior <- RW()
  matrix_along_by <- matrix(0:9, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2", {
  prior <- RW2()
  matrix_along_by <- matrix(0:9, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_spline", {
  prior <- Sp()
  matrix_along_by <- matrix(0:9, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_svd", {
  prior <- SVD(HMD)
  matrix_along_by <- matrix(0:9, nc = 1)
  ans <- draw_vals_hyper(prior = prior,
                         matrix_along_by = matrix_along_by,
                         n_sim = 10)
  expect_identical(ans, list())
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
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_known", {
    expect_true(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                     nm = "sex",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_lin", {
    expect_true(is_prior_ok_for_term(prior = Lin(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws expected error with bage_prior_known", {
    expect_error(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                      nm = "sex",
                                     matrix_along_by = matrix(0:2, nc = 1),
                                      agesex = "other"),
                 "`Known\\(c\\(0.1,-0.1\\)\\)` prior for `sex` term invalid.")    
})

test_that("'is_prior_ok_for_term' works with bage_prior_norm", {
    expect_true(is_prior_ok_for_term(prior = N(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_normfixed", {
    expect_true(is_prior_ok_for_term(prior = NFix(),
                                     nm = "sex",
                                     matrix_along_by = matrix(0, nc = 1),
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw", {
    expect_true(is_prior_ok_for_term(prior = RW(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2", {
    expect_true(is_prior_ok_for_term(prior = RW2(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:2, nc = 1),
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_spline", {
    expect_true(is_prior_ok_for_term(prior = Sp(),
                                     nm = "time",
                                     matrix_along_by = matrix(0:1, nc = 1),
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd, correct inputs", {
    expect_true(is_prior_ok_for_term(prior = SVD(sim_scaled_svd()),
                                     nm = "age:sex",
                                     matrix_along_by = matrix(0:9, nc = 2),
                                     agesex = "age:sex"))
})

test_that("'is_prior_ok_for_term' throws correct error with main effect, agesex NULL", {
    s <- sim_scaled_svd()
    expect_error(is_prior_ok_for_term(prior = SVD(s),
                                     nm = "bla",
                                     matrix_along_by = matrix(0:9, nc = 1),
                                     agesex = NULL),
                "Problem with `SVD\\(s\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' throws correct error with interaction, agesex NULL", {
    s <- sim_scaled_svd()
    expect_error(is_prior_ok_for_term(prior = SVD(s),
                                     nm = "bla:bleh",
                                     matrix_along_by = matrix(0:9, nc = 2),
                                     agesex = NULL),
                "Problem with `SVD\\(s\\)` prior for `bla:bleh` term.")
})

test_that("'is_prior_ok_for_term' throws correct error order-3 interaction, agesex 'other'", {
  s <- sim_scaled_svd()
  expect_error(is_prior_ok_for_term(prior = SVD(s),
                                    nm = "bla:bleh:blu",
                                    matrix_along_by = matrix(0:11, nc = 2),
                                    agesex = "other"),
               "Problem with `SVD\\(s\\)` prior for `bla:bleh:blu` term.")
})


## levels_hyper ---------------------------------------------------------------

test_that("'levels_hyper' works with 'bage_prior_ar'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = AR(n = 2), matrix_along_by = matrix_along_by),
                   c("coef", "coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_ilin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = ILin(), matrix_along_by = matrix_along_by),
                   c("slope", "mslope", "sd", "msd"))
})

test_that("'levels_hyper' works with 'bage_prior_known'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Known(1), matrix_along_by = matrix_along_by),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_lin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Lin(), matrix_along_by = matrix_along_by),
                   c("slope", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_norm'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = N(), matrix_along_by = matrix_along_by),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_normfixed'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = NFix(), matrix_along_by = matrix_along_by),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_rw'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = RW(), matrix_along_by = matrix_along_by),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = RW2(), matrix_along_by = matrix_along_by),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_spline'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Sp(), matrix_along_by = matrix_along_by),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_svd'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = SVD(sim_scaled_svd()),
                                matrix_along_by = matrix_along_by),
                   character())
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
                     "AR(n=2,s=0.3)")
})

test_that("'str_call_prior' works with bage_prior_ilin", {
    expect_identical(str_call_prior(ILin()), "ILin()")
    expect_identical(str_call_prior(ILin(sd = 0.5, s = 2, ms = 1.2, along = "a")),
                     "ILin(s=2,sd=0.5,ms=1.2,along=\"a\")")
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


## uses_along -----------------------------------------------------------------

test_that("'uses_along' works with valid inputs", {
    expect_false(uses_along(N()))
    expect_true(uses_along(ILin()))
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

