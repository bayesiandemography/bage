

## User-visible constructors --------------------------------------------------

expect_identical(N(), new_bage_prior_norm())

## Internal constructors ------------------------------------------------------

test_that("'new_bage_prior_norm' works", {
    obj <- new_bage_prior_norm()
    expect_s3_class(obj, "bage_prior_norm")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$n_hyper, 1L)
    expect_identical(obj$i_prior, 1L)
})
    
