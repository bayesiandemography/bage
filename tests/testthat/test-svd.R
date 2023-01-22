
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
    x <- matrix(rnorm(1000), nr = 20)
    n <- 5L
    ans <- scaled_svd(x = x, n = n)
    expect_identical(dim(ans$transform), c(20L, 5L))
    expect_equal(ans$translate, rep(0, 20), tolerance = 0.1)
})


## 'svd_transform' ------------------------------------------------------------

test_that("'svd_transform' performs correctly with valid inputs - log", {
    set.seed(0)
    x <- expand.grid(age = 0:5,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rgamma(nrow(x), shape = 0.2)
    ans <- svd_transform(x = x, n = 3, concat = c("F", "M"))
    expect_identical(names(ans), c("F", "M", "T", "concat"))
    expect_identical(names(ans[[3L]]), c("transform", "translate"))
    expect_identical(dim(ans$T$transform), c(6L, 3L))
    expect_identical(length(ans$concat$translate), 12L)
    expect_false(any(is.infinite(unlist(ans))))
    expect_false(any(is.na(unlist(ans))))
})

test_that("'svd_transform' performs correctly with valid inputs - logit", {
    set.seed(0)
    x <- expand.grid(age = 0:5,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rbeta(nrow(x), shape1 = 2, shape2 = 3)
    ans <- svd_transform(x = x, n = 3, scale = "logit", concat = c("F", "M"))
    expect_false(any(is.infinite(unlist(ans))))
    expect_false(any(is.na(unlist(ans))))
})

test_that("'svd_transform' performs correctly with valid inputs - none", {
    set.seed(0)
    x <- expand.grid(age = 0:5,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rnorm(nrow(x))
    ans <- svd_transform(x = x, n = 3, scale = "none", concat = c("F", "M"))
    expect_false(any(is.infinite(unlist(ans))))
    expect_false(any(is.na(unlist(ans))))
})




    
    
    



            
