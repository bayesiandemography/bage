

## User-visible constructors --------------------------------------------------

expect_identical(Known(values = 1:3), new_bage_prior_known(values = as.double(1:3)))

expect_identical(N(), new_bage_prior_norm(scale = 1))

expect_identical(RW(), new_bage_prior_rw(scale = 1))

expect_identical(RW2(), new_bage_prior_rw2(scale = 1))


## Internal constructors ------------------------------------------------------

test_that("'new_bage_prior_known' works", {
    obj <- new_bage_prior_known(values = 1.0)
    expect_s3_class(obj, "bage_prior_known")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 0L)
    expect_identical(obj$consts, double())
    expect_identical(obj$n_hyper, 0L)
    expect_identical(obj$specific, list(values = 1.0))
})

test_that("'new_bage_prior_norm' works", {
    obj <- new_bage_prior_norm(scale = 1)
    expect_s3_class(obj, "bage_prior_norm")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 1L)
    expect_identical(obj$consts, 1.0)
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$specific, list(scale = 1))
})

test_that("'new_bage_prior_rw' works", {
    obj <- new_bage_prior_rw(scale = 1)
    expect_s3_class(obj, "bage_prior_rw")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 2L)
    expect_identical(obj$consts, 1.0)
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$specific, list(scale = 1))
})

test_that("'new_bage_prior_rw2' works", {
    obj <- new_bage_prior_rw2(scale = 1)
    expect_s3_class(obj, "bage_prior_rw2")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 3L)
    expect_identical(obj$consts, 1.0)
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$specific, list(scale = 1))
})

