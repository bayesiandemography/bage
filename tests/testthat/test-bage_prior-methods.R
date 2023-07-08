
## is_known -------------------------------------------------------------------

test_that("'is_known' works with valid inputs", {
    expect_false(is_known(N()))
    expect_true(is_known(Known(values = 1:3)))
})


## levels_const ---------------------------------------------------------------

test_that("'levels_const' works with 'bage_prior_ar1'", {
    expect_identical(levels_const(AR1()), c("shape1", "shape2", "min", "max", "scale"))
})

test_that("'levels_const' works with 'bage_prior_known'", {
    expect_identical(levels_const(Known(1)), character())
})

test_that("'levels_const' works with 'bage_prior_norm'", {
    expect_identical(levels_const(N()), "scale")
})

test_that("'levels_const' works with 'bage_prior_normfixed'", {
    expect_identical(levels_const(NFixed()), "sd")
})

test_that("'levels_const' works with 'bage_prior_rw'", {
    expect_identical(levels_const(RW()), "scale")
})

test_that("'levels_const' works with 'bage_prior_rw2'", {
    expect_identical(levels_const(RW2()), "scale")
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
    expect_identical(levels_hyper(NFixed()), character())
})

test_that("'levels_hyper' works with 'bage_prior_rw'", {
    expect_identical(levels_hyper(RW()), "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2'", {
    expect_identical(levels_hyper(RW2()), "sd")
})


## 'str_call_prior' -----------------------------------------------------------

test_that("'str_call_prior' works with bage_prior_ar1", {
    expect_identical(str_call_prior(AR1()), "AR1()")
    expect_identical(str_call_prior(AR1(min = 0.5)), "AR1(min=0.5)")
    expect_identical(str_call_prior(AR1(max = 0.95)), "AR1(max=0.95)")
    expect_identical(str_call_prior(AR1(scale = 0.3)), "AR1(scale=0.3)")
    expect_identical(str_call_prior(AR1(min = 0.5, max = 0.95, scale = 0.3)),
                     "AR1(min=0.5, max=0.95, scale=0.3)")
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
    expect_identical(str_call_prior(N(scale = 0.95)), "N(scale=0.95)")
})

test_that("'str_call_prior' works with bage_prior_normfixed", {
    expect_identical(str_call_prior(NFixed()), "NFixed()")
    expect_identical(str_call_prior(NFixed(sd = 0.95)), "NFixed(sd=0.95)")
})

test_that("'str_call_prior' works with bage_prior_rw", {
    expect_identical(str_call_prior(RW()), "RW()")
    expect_identical(str_call_prior(RW(scale = 0.95)), "RW(scale=0.95)")
})

test_that("'str_call_prior' works with bage_prior_rw", {
    expect_identical(str_call_prior(RW2()), "RW2()")
    expect_identical(str_call_prior(RW2(scale = 0.95)), "RW2(scale=0.95)")
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
    l <- transform_hyper(NFixed())
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


## values_known ---------------------------------------------------------------

test_that("'values_known' works with valid inputs", {
    expect_identical(values_known(Known(values = 1:3)),
                     as.double(1:3))
})

