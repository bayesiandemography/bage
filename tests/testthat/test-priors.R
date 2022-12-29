

## User-visible constructors --------------------------------------------------

expect_identical(N(), new_bage_prior_norm())

expect_identical(RW(), new_bage_prior_rw())

expect_identical(RW2(), new_bage_prior_rw2())


## Internal constructors ------------------------------------------------------

test_that("'new_bage_prior_norm' works", {
    obj <- new_bage_prior_norm()
    expect_s3_class(obj, "bage_prior_norm")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$i_prior, 1L)
})

test_that("'new_bage_prior_rw' works", {
    obj <- new_bage_prior_rw()
    expect_s3_class(obj, "bage_prior_rw")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$i_prior, 2L)
})

test_that("'new_bage_prior_rw2' works", {
    obj <- new_bage_prior_rw2()
    expect_s3_class(obj, "bage_prior_rw2")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$i_prior, 3L)
})

