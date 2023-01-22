

## 'prepare_svd_probs' ------------------------------------------------------------

test_that("'prepare_svd_probs' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 1), nrow = 5)
    x[sample(100, 5)] <- 0
    x[sample(100, 5)] <- 1
    ans_obtained <- prepare_svd_probs(x)
    expect_true(all(is.finite(ans_obtained)))
    expect_true(!anyNA(ans_obtained))
})


## 'prepare_svd_rates' ------------------------------------------------------------

test_that("'prepare_svd_rates' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 2), nrow = 5)
    x[sample(100, 10)] <- 0
    ans_obtained <- prepare_svd_rates(x)
    expect_true(all(is.finite(ans_obtained)))
    expect_true(!anyNA(ans_obtained))
})


## 'replace_zeros' ------------------------------------------------------------

test_that("'replace_zeros' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 2), nrow = 5)
    x[sample(100, 10)] <- 0
    ans_obtained <- replace_zeros(x)
    expect_true(all(ans_obtained > 0))
    expect_true(all(ans_obtained[x > 0] == x[x > 0]))
})


## 'replace_zeros_ones' -------------------------------------------------------

test_that("'replace_zeros_ones' performs correctly with valid inputs", {
    x <- matrix(runif(n = 100, min = 0, max = 1), nrow = 5)
    x[sample(100, 5)] <- 0
    x[sample(100, 5)] <- 1
    ans_obtained <- replace_zeros_ones(x)
    expect_true(all(ans_obtained > 0))
    expect_true(all(ans_obtained < 1))
    expect_true(all(ans_obtained[(x > 0) & (x < 1)] == x[(x > 0) & (x < 1)]))
})


## 'scaled_svd' ---------------------------------------------------------------

test_that("'scaled_svd' performs correctly with valid inputs", {
    



            
