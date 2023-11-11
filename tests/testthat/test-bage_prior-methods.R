
## draw_vals_hyper ------------------------------------------------------------

test_that("'draw_vals_hyper' works with bage_prior_ar1", {
    prior <- AR1()
    ans <- draw_vals_hyper(prior = prior, n_sim = 10)
    expect_identical(names(ans), c("coef", "sd"))
    expect_identical(length(ans$coef), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_known", {
    prior <- Known(c(0.1, 0.3))
    ans <- draw_vals_hyper(prior = prior, n_sim = 10)
    expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_norm", {
    prior <- N()
    ans <- draw_vals_hyper(prior = prior, n_sim = 10)
    expect_identical(names(ans), "sd")
    expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_normfixed", {
    prior <- NFix()
    ans <- draw_vals_hyper(prior = prior, n_sim = 10)
    expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_rw", {
    prior <- RW()
    ans <- draw_vals_hyper(prior = prior, n_sim = 10)
    expect_identical(names(ans), "sd")
    expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2", {
    prior <- RW2()
    ans <- draw_vals_hyper(prior = prior, n_sim = 10)
    expect_identical(names(ans), "sd")
    expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_spline", {
    prior <- Spline()
    ans <- draw_vals_hyper(prior = prior, n_sim = 10)
    expect_identical(names(ans), "sd")
    expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_svd", {
    prior <- SVD(HMD)
    ans <- draw_vals_hyper(prior = prior, n_sim = 10)
    expect_identical(ans, list())
})


## draw_vals_effect --------------------------------------------------------------

test_that("'draw_vals_effect' works with bage_prior_ar1", {
    prior <- AR1()
    n_sim <- 10
    vals_hyper <- draw_vals_hyper(prior = prior,
                                  n_sim = n_sim)
    levels_effect <- letters
    ans <- draw_vals_effect(prior = prior,
                         vals_hyper = vals_hyper,
                         levels_effect = levels_effect,
                         agesex = NULL,
                         n_sim = n_sim)
    expect_identical(dimnames(ans), list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_known", {
    prior <- Known(c(-0.1, 0, 0.1))
    n_sim <- 10
    levels_effect <- c("a", "b", "c")
    ans <- draw_vals_effect(prior = prior,
                         vals_hyper = NULL,
                         levels_effect = levels_effect,
                         agesex = NULL,
                         n_sim = n_sim)
    expect_identical(dimnames(ans), list(c("a", "b", "c"), as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_norm", {
    prior <- N()
    n_sim <- 10
    vals_hyper <- draw_vals_hyper(prior = prior,
                                  n_sim = n_sim)
    levels_effect <- seq_len(1000)
    ans <- draw_vals_effect(prior = prior,
                         vals_hyper = vals_hyper,
                         levels_effect = levels_effect,
                         agesex = NULL,
                         n_sim = n_sim)
    expect_identical(dimnames(ans),
                     list(as.character(1:1000), as.character(1:10)))
    expect_equal(unname(apply(ans, 2, sd)), vals_hyper$sd, tolerance = 0.05)
})

test_that("'draw_vals_effect' works with bage_prior_normfixed", {
    prior <- NFix(sd = 0.3)
    n_sim <- 10
    levels_effect <- seq_len(1000)
    ans <- draw_vals_effect(prior = prior,
                         vals_hyper = NULL,
                         levels_effect = levels_effect,
                         agesex = NULL,
                         n_sim = n_sim)
    expect_identical(dimnames(ans),
                     list(as.character(1:1000), as.character(1:10)))
    expect_equal(unname(apply(ans, 2, sd)), rep(0.3, 10), tolerance = 0.05)
})

test_that("'draw_vals_effect' works with bage_prior_rw", {
    prior <- RW()
    n_sim <- 10
    vals_hyper <- draw_vals_hyper(prior = prior, n_sim = n_sim)
    levels_effect <- letters
    ans <- draw_vals_effect(prior = prior,
                         vals_hyper = vals_hyper,
                         levels_effect = levels_effect,
                         agesex = NULL,
                         n_sim = n_sim)
    expect_identical(dimnames(ans),
                     list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_rw2", {
    prior <- RW2()
    n_sim <- 10
    vals_hyper <- draw_vals_hyper(prior = prior, n_sim = n_sim)
    levels_effect <- letters
    ans <- draw_vals_effect(prior = prior,
                         vals_hyper = vals_hyper,
                         levels_effect = levels_effect,
                         agesex = NULL,
                         n_sim = n_sim)
    expect_identical(dimnames(ans),
                     list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_spline", {
    prior <- Spline()
    n_sim <- 10
    vals_hyper <- draw_vals_hyper(prior = prior, n_sim = n_sim)
    levels_effect <- letters
    ans <- draw_vals_effect(prior = prior,
                         vals_hyper = vals_hyper,
                         levels_effect = levels_effect,
                         agesex = NULL,
                         n_sim = n_sim)
    expect_identical(dimnames(ans),
                     list(letters, as.character(1:10)))
})

test_that("'draw_vals_effect' works with bage_prior_svd", {
    prior <- SVD(HMD)
    n_sim <- 10
    vals_hyper <- draw_vals_hyper(prior = prior, n_sim = n_sim)
    levels_effect <- c(0:79, "80+")
    ans <- draw_vals_effect(prior = prior,
                         vals_hyper = vals_hyper,
                         levels_effect = levels_effect,
                         agesex = "age",
                         n_sim = n_sim)
    expect_identical(dimnames(ans),
                     list(levels_effect, as.character(1:10)))
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
                                     length_effect = 4L,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_known", {
    expect_true(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                     nm = "sex",
                                     length_effect = 2,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' throws expected error with bage_prior_known", {
    expect_error(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                      nm = "sex",
                                      length_effect = 3,
                                      agesex = "other"),
                 "`Known\\(c\\(0.1,-0.1\\)\\)` prior for `sex` term invalid.")    
})

test_that("'is_prior_ok_for_term' works with bage_prior_norm", {
    expect_true(is_prior_ok_for_term(prior = N(),
                                     nm = "sex",
                                     length_effect = 2,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_normfixed", {
    expect_true(is_prior_ok_for_term(prior = NFix(),
                                     nm = "sex",
                                     length_effect = 1,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw", {
    expect_true(is_prior_ok_for_term(prior = RW(),
                                     nm = "time",
                                     length_effect = 2,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2", {
    expect_true(is_prior_ok_for_term(prior = RW2(),
                                     nm = "time",
                                     length_effect = 3,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_spline", {
    expect_true(is_prior_ok_for_term(prior = Spline(),
                                     nm = "time",
                                     length_effect = 2,
                                     agesex = "other"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd, correct inputs", {
    expect_true(is_prior_ok_for_term(prior = SVD(sim_scaled_svd()),
                                     nm = "age:sex",
                                     length_effect = 10,
                                     agesex = "age:sex"))
})

test_that("'is_prior_ok_for_term' throws correct error with main effect, agesex NULL", {
    s <- sim_scaled_svd()
    expect_error(is_prior_ok_for_term(prior = SVD(s),
                                     nm = "bla",
                                     length_effect = 10,
                                     agesex = NULL),
                "Problem with `SVD\\(s\\)` prior for `bla` term.")
})

test_that("'is_prior_ok_for_term' throws correct error with interaction, agesex NULL", {
    s <- sim_scaled_svd()
    expect_error(is_prior_ok_for_term(prior = SVD(s),
                                     nm = "bla:bleh",
                                     length_effect = 10,
                                     agesex = NULL),
                "Problem with `SVD\\(s\\)` prior for `bla:bleh` term.")
})

test_that("'is_prior_ok_for_term' throws correct error order-3 interaction, agesex 'other'", {
    s <- sim_scaled_svd()
    expect_error(is_prior_ok_for_term(prior = SVD(s),
                                      nm = "bla:bleh:blu",
                                      length_effect = 10,
                                      agesex = "other"),
                 "Problem with `SVD\\(s\\)` prior for `bla:bleh:blu` term.")
})


## levels_hyper ---------------------------------------------------------------

test_that("'levels_hyper' works with 'bage_prior_ar1'", {
    expect_identical(levels_hyper(AR1()), c("coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_known'", {
    expect_identical(levels_hyper(Known(1)), character())
})

test_that("'levels_hyper' works with 'bage_prior_norm'", {
    expect_identical(levels_hyper(N()), "sd")
})

test_that("'levels_hyper' works with 'bage_prior_normfixed'", {
    expect_identical(levels_hyper(NFix()), character())
})

test_that("'levels_hyper' works with 'bage_prior_rw'", {
    expect_identical(levels_hyper(RW()), "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2'", {
    expect_identical(levels_hyper(RW2()), "sd")
})

test_that("'levels_hyper' works with 'bage_prior_spline'", {
    expect_identical(levels_hyper(Spline()), "sd")
})

test_that("'levels_hyper' works with 'bage_prior_svd'", {
    expect_identical(levels_hyper(SVD(sim_scaled_svd())), character())
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
    prior <- Spline(n = 5)
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
    prior <- Spline()
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

test_that("'str_call_prior' works with bage_prior_ar1", {
    expect_identical(str_call_prior(AR1()), "AR1()")
    expect_identical(str_call_prior(AR1(min = 0.5)), "AR1(min=0.5)")
    expect_identical(str_call_prior(AR1(max = 0.95)), "AR1(max=0.95)")
    expect_identical(str_call_prior(AR1(s = 0.3)), "AR1(s=0.3)")
    expect_identical(str_call_prior(AR1(min = 0.5, max = 0.95, s = 0.3)),
                     "AR1(min=0.5, max=0.95, s=0.3)")
})

test_that("'str_call_prior' works with bage_prior_ar1", {
    expect_identical(str_call_prior(Known(1)), "Known(1)")
    expect_identical(str_call_prior(Known(c(2, 3, -2, 0))),
                                    "Known(c(2,3,-2,0))")
    expect_identical(str_call_prior(Known(c(2, 3, -2, 0,7, 3))),
                                    "Known(c(2,...,3))")
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
    expect_identical(str_call_prior(RW2(flat = TRUE)), "RW2(flat=TRUE)")
    expect_identical(str_call_prior(RW2(s = 0.95)), "RW2(s=0.95)")
    expect_identical(str_call_prior(RW2(flat = TRUE, s = 0.95)), "RW2(s=0.95,flat=TRUE)")
})

test_that("'str_call_prior' works with bage_prior_spline", {
    expect_identical(str_call_prior(Spline()), "Spline()")
    expect_identical(str_call_prior(Spline(n = 5L)), "Spline(n=5)")
    expect_identical(str_call_prior(Spline(s = 0.1)), "Spline(s=0.1)")
    expect_identical(str_call_prior(Spline(s = 3,n = 5L)), "Spline(n=5,s=3)")
})

test_that("'str_call_prior' works with bage_prior_svd", {
    s <- sim_scaled_svd()
    expect_identical(str_call_prior(SVD(s)), "SVD(s)")
    expect_identical(str_call_prior(SVD(s,indep = FALSE)), "SVD(s,indep=FALSE)")
    expect_identical(str_call_prior(SVD(s,n = 6L)), "SVD(s,n=6)")
    expect_identical(str_call_prior(SVD(s,indep=F,n = 3L)), "SVD(s,n=3,indep=FALSE)")
})


## transform_hyper ------------------------------------------------------------

test_that("'transform_hyper' works with 'bage_prior_ar1'", {
    logit <- function(x) log(x / (1 - x))
    l <- transform_hyper(AR1())
    expect_equal(0.35, l[[1]](logit(0.35)))
    expect_equal(0.35, l[[2]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_known'", {
    l <- transform_hyper(Known(1))
    expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_norm'", {
    l <- transform_hyper(N())
    expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_normfixed'", {
    l <- transform_hyper(NFix())
    expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_rw'", {
    l <- transform_hyper(RW())
    expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2'", {
    l <- transform_hyper(RW2())
    expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_spline'", {
    l <- transform_hyper(Spline())
    expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_svd'", {
    l <- transform_hyper(SVD(HMD))
    expect_identical(l, list())
})


## uses_matrix_effectfree_effect ----------------------------------------------------

test_that("'uses_matrix_effectfree_effect' works with valid inputs", {
    expect_false(uses_matrix_effectfree_effect(N()))
    expect_true(uses_matrix_effectfree_effect(Spline()))
    expect_true(uses_matrix_effectfree_effect(SVD(HMD)))
})


## uses_offset_effectfree_effect ----------------------------------------------------

test_that("'uses_offset_effectfree_effect' works with valid inputs", {
    expect_false(uses_offset_effectfree_effect(N()))
    expect_false(uses_offset_effectfree_effect(Spline()))
    expect_true(uses_offset_effectfree_effect(SVD(HMD)))
})


## values_known ---------------------------------------------------------------

test_that("'values_known' works with valid inputs", {
    expect_identical(values_known(Known(values = 1:3)),
                     as.double(1:3))
})

