
## 'scaled_svd_comp' ----------------------------------------------------------

test_that("'scaled_svd_comp' performs correctly with valid inputs - log", {
    set.seed(0)
    x <- expand.grid(age = 0:5,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rgamma(nrow(x), shape = 0.2)
    x <- poputils::to_matrix(x,
                             rows = c(age, sex),
                             cols = c(country, year), measure = value)
    ans <- scaled_svd_comp(x = x, n = 3, transform = "log")
    expect_identical(names(ans), c("matrix", "offset"))
    expect_identical(dim(ans$matrix), c(18L, 3L))
    expect_identical(length(ans$offset), 18L)
    expect_false(any(vapply(ans, anyNA, FALSE)))
    expect_false(any(vapply(ans, function(x) any(is.infinite(x)), FALSE)))
})

test_that("'scaled_svd_comp' performs correctly with valid inputs - logit", {
    set.seed(0)
    x <- expand.grid(age = 0:5,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rbeta(nrow(x), shape1 = 2, shape2 = 3)
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    ans <- scaled_svd_comp(x = x, n = 3, transform = "logit")
    expect_false(any(sapply(ans, function(x) any(is.infinite(x)))))
    expect_false(any(sapply(ans, anyNA)))
})

test_that("'scaled_svd_comp' performs correctly with valid inputs - none", {
    set.seed(0)
    x <- expand.grid(age = 0:5,
                     sex = c("F", "M", "T"),
                     year = 2000:2001,
                     country = letters)
    x$value <- rnorm(nrow(x))
    x <- poputils::to_matrix(x, rows = age, cols = c(sex, year, country), measure = value)
    ans <- scaled_svd_comp(x = x, n = 3, transform = "none")
    expect_false(any(sapply(ans, function(x) any(is.infinite(x)))))
    expect_false(any(sapply(ans, anyNA)))
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


    
    
    



         
