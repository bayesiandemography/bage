

## 'get_n_hyper' --------------------------------------------------------------

test_that("'get_n_hyper' works with valid inputs", {
    prior <- N()
    expect_identical(get_n_hyper(prior), 1L)
})


## 'make_hyper' ---------------------------------------------------------------

test_that("'make_hyper' works with valid inputs", {
    ans_obtained <- make_hyper(list(a = N(), b = RW(), c = N()))
    ans_expected <- rep(0, 3L)
    expect_true(is.double(ans_expected))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_index_hyper' ---------------------------------------------------------

test_that("'make_index_hyper' works with valid inputs", {
    ans_obtained <- make_index_hyper(list(a = N(), b = RW(), c = N()))
    ans_expected <- 0:2
    expect_identical(ans_obtained, ans_expected)
})


## 'make_index_par' -----------------------------------------------------------

test_that("'make_index_par' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- 1
    outcome <- xtabs(deaths ~ age + sex + time, data = data)
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_index_par(formula = formula, outcome = outcome)
    ans_expected <- rep(0:2, times = c(1, 2, 6))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_matrices_par' --------------------------------------------------------

test_that("'make_matrices_par' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- 1
    outcome <- xtabs(deaths ~ age + sex + time, data = data)
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_matrices_par(formula = formula, outcome = outcome)
    ans_expected <- list("(Intercept)" = as(matrix(integer(), nr = 0), "sparseMatrix"),
                         "time" = as(matrix(rep(c(1L, 0L, 0L, 1L), each = 6), nr = 12),
                                     "sparseMatrix"),
                         "age:sex" = as(rbind(diag(6), diag(6)), "sparseMatrix"))
    expect_identical(ans_obtained, ans_expected)
})
                                        
    
## 'make_matrix_par' ----------------------------------------------------------

test_that("'make_matrix_par' works with one-dimensional term and 3-dimensional array", {
    dim <- 2:4
    ## 1
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(TRUE, FALSE, FALSE))
    beta <- rnorm(2)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[,,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
    ## 2
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(FALSE, TRUE, FALSE))
    beta <- rnorm(3)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[1,,1] <- beta
    ans_expected[2,,1] <- beta
    ans_expected[1,,2] <- beta
    ans_expected[2,,2] <- beta
    ans_expected[1,,3] <- beta
    ans_expected[2,,3] <- beta
    ans_expected[1,,4] <- beta
    ans_expected[2,,4] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
    ## 3
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(FALSE, FALSE, TRUE))
    beta <- rnorm(4)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[1,1,] <- beta
    ans_expected[2,1,] <- beta
    ans_expected[1,2,] <- beta
    ans_expected[2,2,] <- beta
    ans_expected[1,3,] <- beta
    ans_expected[2,3,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
})

test_that("'make_matrix_par' works with two-dimensional term and 3-dimensional array", {
    dim <- 2:4
    ## 1 and 2
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(TRUE, TRUE, FALSE))
    beta <- rnorm(6)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[,,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
    ## 1 and 3
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(TRUE, FALSE, TRUE))
    beta <- rnorm(8)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[,1,] <- beta
    ans_expected[,2,] <- beta
    ans_expected[,3,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
    ## 2 and 3
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(FALSE, TRUE, TRUE))
    beta <- rnorm(12)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[1,,] <- beta
    ans_expected[2,,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
})

test_that("'make_matrix_par' works with 3-dimensional term and 3-dimensional array", {
    dim <- 2:4
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(TRUE, TRUE, TRUE))
    beta <- rnorm(24)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[,,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
})

test_that("'make_matrix_par' works with one-dimensional term and one-dimensional array", {
    dim <- 4
    m <- make_matrix_par(dim = dim,
                         is_in_term = TRUE)
    beta <- rnorm(4)
    ans_obtained <- m %*% beta
    ans_expected <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
})

test_that("'make_matrix_par' creates sparse matrix", {
    dim <- 2:4
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(TRUE, FALSE, FALSE))
    expect_s4_class(m, "sparseMatrix")
    dim <- 2:4
    m <- make_matrix_par(dim = dim,
                         is_in_term = c(TRUE, FALSE, TRUE))
    expect_s4_class(m, "sparseMatrix")
})


## 'make_priors' --------------------------------------------------------------

test_that("'make_priors' works with valid inputs - has intercept", {
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_priors(formula)
    ans_expected <- list("(Intercept)" = N(scale = 10),
                         time = N(),
                         "age:sex" = N())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_priors' works with valid inputs - no intercept", {
    formula <- deaths ~ age:sex + time - 1
    ans_obtained <- make_priors(formula)
    ans_expected <- list(time = N(),
                         "age:sex" = N())
    expect_identical(ans_obtained, ans_expected)
})


## 'make_outcome' -------------------------------------------------------------

test_that("'make_outcome' works with valid inputs - nm_distn not 'norm'", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- seq_len(nrow(data))
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_outcome(formula = formula,
                                 data = data,
                                 nm_distn = "pois")
    ans_expected <- xtabs(deaths ~ age + sex + time, data = data)
    ans_expected <- array(1 * ans_expected,
                          dim = dim(ans_expected),
                          dimnames = dimnames(ans_expected))
    expect_identical(ans_obtained, ans_expected)
    expect_identical(names(dimnames(ans_obtained)), c("age", "sex", "time"))
})

test_that("'make_outcome' works with valid inputs - nm_distn is 'norm'", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- seq_len(nrow(data))
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_outcome(formula = formula,
                                 data = data,
                                 nm_distn = "norm")
    ans_expected <- xtabs(deaths ~ age + sex + time, data = data)
    ans_expected <- array(1 * ans_expected,
                          dim = dim(ans_expected),
                          dimnames = dimnames(ans_expected))
    ans_expected <- (ans_expected - mean(ans_expected)) / sd(ans_expected)
    expect_identical(ans_obtained, ans_expected)
    expect_identical(names(dimnames(ans_obtained)), c("age", "sex", "time"))
    expect_equal(mean(ans_expected), 0)
    expect_equal(sd(ans_expected), 1)
})

