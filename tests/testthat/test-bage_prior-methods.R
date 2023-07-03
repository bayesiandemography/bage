
## is_known -------------------------------------------------------------------

test_that("'is_known' works with valid inputs", {
    expect_false(is_known(N()))
    expect_true(is_known(Known(values = 1:3)))
})


## length_parfree ---------------------------------------------------------------

test_that("'length_parfree' works with 'bage_prior_norm'", {
    expect_identical(length_parfree(N(), 10L), 10L)
})

test_that("'length_parfree' works with 'bage_prior_norm'", {
    expect_identical(length_parfree(RW(), 10L), 9L)
})

test_that("'length_parfree' works with 'bage_prior_norm'", {
    expect_identical(length_parfree(RW2(), 10L), 8L)
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

test_that("'levels_hyper' works with 'bage_prior_rw'", {
    expect_identical(levels_hyper(RW()), "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2'", {
    expect_identical(levels_hyper(RW2()), "sd")
})


## make_matrix_parfree --------------------------------------------------------

test_that("'make_matrix_parfree' works with 'bage_prior_norm'", {
    expect_identical(make_matrix_parfree(N(), 3),
                     Matrix::Diagonal(n = 3, x = 1L))
})

test_that("'make_matrix_parfree' works with 'bage_prior_rw'", {
    m <- make_matrix_parfree(RW(), length_par = 6)
    expect_identical(dim(m), 6:5)
    expect_equal(sum(m %*% (1:5)), 0)
    expect_equal(diff(as.numeric(m %*% (1:5))), 1:5)
})

test_that("'make_matrix_parfree' works with 'bage_prior_rw2'", {
    set.seed(0)
    m <- make_matrix_parfree(RW2(), length_par = 6)
    expect_identical(dim(m), c(6L, 4L))
    x <- rnorm(4)
    expect_equal(diff(as.numeric(m %*% x), differences = 2), x)
    expect_equal(sum(m %*% x), 0)
    expect_false(sum(diff(as.numeric(m %*% x))) == 0)
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

