
## is_known -------------------------------------------------------------------

test_that("'is_known' works with valid inputs", {
    expect_false(is_known(N()))
    expect_true(is_known(Known(values = 1:3)))
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

test_that("'levels_hyper' works with 'bage_prior_rw'", {
    expect_identical(levels_hyper(RW()), "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2'", {
    expect_identical(levels_hyper(RW2()), "sd")
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

test_that("'transform_hyper' works with 'bage_prior_rw'", {
    l <- transform_hyper(RW())
    expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2'", {
    l <- transform_hyper(RW2())
    expect_equal(0.35, l[[1]](log(0.35)))
})


## values_known ---------------------------------------------------------------

test_that("'values_known' works with valid inputs", {
    expect_identical(values_known(Known(values = 1:3)),
                     as.double(1:3))
})

