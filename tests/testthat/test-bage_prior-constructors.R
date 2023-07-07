

## User-visible constructors --------------------------------------------------

test_that("'AR1' works with valid inputs", {
    expect_identical(AR1(), new_bage_prior_ar1(min = 0.8, max = 0.98, scale = 1))
})

test_that("'AR1' throws current error when min >= max", {
    expect_error(AR1(min = 0.8, max = 0.8),
                 "'min' \\[0.8\\] greater than or equal to 'max' \\[0.8\\]")
})

test_that("'Known' works with valid inputs", {
    expect_identical(Known(values = 1:3),
                     new_bage_prior_known(values = as.double(1:3)))
})

test_that("'N' works with valid inputs", {
    expect_identical(N(), new_bage_prior_norm(scale = 1))
})

test_that("'NFixed' works with valid inputs", {
    expect_identical(NFixed(), new_bage_prior_normfixed(sd = 1))
})

test_that("'RW' works with valid inputs", {
    expect_identical(RW(), new_bage_prior_rw(scale = 1))
})

test_that("'RW2' works with valid inputs", {
    expect_identical(RW2(), new_bage_prior_rw2(sd = 1, scale = 1))
})


## Internal constructors ------------------------------------------------------

test_that("'new_bage_prior_ar1' works", {
    obj <- new_bage_prior_ar1(min = 0.8, max = 0.98, scale = 1.0)
    expect_s3_class(obj, "bage_prior_ar1")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 5L)
    expect_identical(obj$const, c(2, 2, 0.8, 0.98, 1))
    expect_identical(obj$n_hyper, 2L)
    expect_identical(obj$specific, list(shape1 = 2,
                                        shape2 = 2,
                                        min = 0.8,
                                        max = 0.98,
                                        scale = 1))
})

test_that("'new_bage_prior_known' works", {
    obj <- new_bage_prior_known(values = 1.0)
    expect_s3_class(obj, "bage_prior_known")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 0L)
    expect_identical(obj$const, double())
    expect_identical(obj$n_hyper, 0L)
    expect_identical(obj$specific, list(values = 1.0))
})

test_that("'new_bage_prior_norm' works", {
    obj <- new_bage_prior_norm(scale = 1)
    expect_s3_class(obj, "bage_prior_norm")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 1L)
    expect_identical(obj$const, 1.0)
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$specific, list(scale = 1))
})

test_that("'new_bage_prior_normfixed' works", {
    obj <- new_bage_prior_normfixed(sd = 1)
    expect_s3_class(obj, "bage_prior_normfixed")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 2L)
    expect_identical(obj$const, 1.0)
    expect_identical(obj$n_hyper, 0L)
    expect_identical(obj$specific, list(sd = 1))
})

test_that("'new_bage_prior_rw' works", {
    obj <- new_bage_prior_rw(scale = 1)
    expect_s3_class(obj, "bage_prior_rw")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 3L)
    expect_identical(obj$const, 1.0)
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$specific, list(scale = 1))
})

test_that("'new_bage_prior_rw2' works", {
    obj <- new_bage_prior_rw2(sd = 1, scale = 1)
    expect_s3_class(obj, "bage_prior_rw2")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 4L)
    expect_identical(obj$const, c(1.0, 1.0))
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$specific, list(sd = 1, scale = 1))
})

