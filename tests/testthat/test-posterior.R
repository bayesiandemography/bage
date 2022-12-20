
## 'make_age_effect' ----------------------------------------------------------

test_that("'make_age_effect' works with single age effect", {
    terms <- list(rnorm(4))
    dim <- integer()
    mappings <- list(integer())
    b <- 1:10
    X <- matrix(rnorm(40), nrow = 10)
    ans_obtained <- make_age_effect(terms = terms,
                                        dim = dim,
                                        mappings = mappings,
                                        b = b,
                                        X = X)
    ans_expected <- array(X %*% terms[[1L]] + b, dim = 10)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_age_effect' works with age effect and two-way interaction", {
    terms <- list(rnorm(4), rnorm(12))
    dim <- 3L
    mappings <- list(integer(), 0L)
    b <- 1:10
    X <- matrix(rnorm(40), nrow = 10)
    ans_obtained <- make_age_effect(terms = terms,
                                        dim = dim,
                                        mappings = mappings,
                                        b = b,
                                        X = X)
    age_effect <- X %*% terms[[1L]]
    age_interact <- cbind(X %*% matrix(terms[[2]], nr = 4)[,1],
                          X %*% matrix(terms[[2]], nr = 4)[,2],
                          X %*% matrix(terms[[2]], nr = 4)[,3])
    ans_expected <- as.numeric(age_effect) + age_interact + b
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_age_effect' works with age effect and two two-way interactions", {
    terms <- list(rnorm(4), rnorm(12), rnorm(20))
    dim <- c(3L, 5L)
    mappings <- list(integer(), 0L, 1L)
    b <- 1:10
    X <- matrix(rnorm(40), nrow = 10)
    ans_obtained <- make_age_effect(terms = terms,
                                        dim = dim,
                                        mappings = mappings,
                                        b = b,
                                        X = X)
    age_effect <- X %*% terms[[1L]]
    age_interact_1 <- cbind(X %*% matrix(terms[[2]], nr = 4)[,1],
                            X %*% matrix(terms[[2]], nr = 4)[,2],
                            X %*% matrix(terms[[2]], nr = 4)[,3])
    age_interact_2 <- cbind(X %*% matrix(terms[[3]], nr = 4)[,1],
                            X %*% matrix(terms[[3]], nr = 4)[,2],
                            X %*% matrix(terms[[3]], nr = 4)[,3],
                            X %*% matrix(terms[[3]], nr = 4)[,4],
                            X %*% matrix(terms[[3]], nr = 4)[,5])[rep(1:10 + rep((0:4) * 10, each = 30))]
    ans_expected <- array(as.numeric(age_effect) + as.numeric(age_interact_1) + age_interact_2 + b, dim = c(10, 3, 5))
    expect_equal(ans_obtained, ans_expected)
})


test_that("'make_age_effect' works with age effect and a two-way interaction and a three-way interaction", {
    terms <- list(rnorm(4), rnorm(12), rnorm(24))
    dim <- c(3L, 2L)
    mappings <- list(integer(), 0L, 0:1)
    b <- 1:10
    X <- matrix(rnorm(40), nrow = 10)
    ans_obtained <- make_age_effect(terms = terms,
                                        dim = dim,
                                        mappings = mappings,
                                        b = b,
                                        X = X)
    age_effect <- X %*% terms[[1L]]
    age_interact_1 <- cbind(X %*% matrix(terms[[2]], nr = 4)[,1],
                            X %*% matrix(terms[[2]], nr = 4)[,2],
                            X %*% matrix(terms[[2]], nr = 4)[,3])
    age_interact_2 <- cbind(X %*% array(terms[[3]], dim = c(4, 3, 2))[,1,1],
                            X %*% array(terms[[3]], dim = c(4, 3, 2))[,2,1],
                            X %*% array(terms[[3]], dim = c(4, 3, 2))[,3,1],
                            X %*% array(terms[[3]], dim = c(4, 3, 2))[,1,2],
                            X %*% array(terms[[3]], dim = c(4, 3, 2))[,2,2],
                            X %*% array(terms[[3]], dim = c(4, 3, 2))[,3,2])
    ans_expected <- array(as.numeric(age_effect) + as.numeric(age_interact_1) + age_interact_2 + b, dim = c(10, 3, 2))
    expect_equal(ans_obtained, ans_expected)
})


## 'make_linear_pred' ---------------------------------------------------------

test_that("'make_linear_pred' works with single intercept", {
    terms <- list(rnorm(1))
    dim <- c(2L, 4L)
    mappings <- list(integer())
    ans_obtained <- make_linear_pred(terms = terms,
                                     dim = dim,
                                     mappings = mappings)
    ans_expected <- array(terms[[1]], dim = dim)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_linear_pred' works single main effect", {
    terms <- list(rnorm(4))
    dim <- c(3L, 4L, 2L)
    mappings <- list(1L)
    ans_obtained <- make_linear_pred(terms = terms,
                                        dim = dim,
                                        mappings = mappings)
    ans_expected <- array(rep(terms[[1]], each = 3), dim = dim)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_linear_pred' works with age effect and two two-way interactions, with one in reverse order", {
    terms <- list(rnorm(3), rnorm(12), rnorm(20))
    dim <- 3:5
    mappings <- list(0L, 1:0, 1:2) 
    ans_obtained <- make_linear_pred(terms = terms,
                                     dim = dim,
                                     mappings = mappings)
    ans_expected <- array(terms[[1]] +
                          as.numeric(t(matrix(terms[[2]], 4))) +
                          rep(terms[[3]], each = 3),
                          dim = dim)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_linear_pred' works with single interaction in wrong order", {
    terms <- list(rnorm(60))
    dim <- 3:5
    mappings <- list(c(1L, 2L, 0L))
    ans_obtained <- make_linear_pred(terms = terms,
                                     dim = dim,
                                     mappings = mappings)
    ans_expected <- aperm(array(terms[[1]], c(4, 5, 3)),
                          perm = c(3, 1, 2))
    expect_identical(ans_obtained, ans_expected)
})





                     
    
