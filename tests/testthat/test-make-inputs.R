
## 'infer_name_age' -----------------------------------------------------------

test_that("'infer_name_age' returns name when single valid answer", {
    expect_identical(infer_name_age(c("age", "sex", "time")),
                     "age")
    expect_identical(infer_name_age(c("Age", "sex", "time")),
                     "Age")
    expect_identical(infer_name_age(c("sex", "time", "AGE_GROUP")),
                     "AGE_GROUP")
    expect_identical(infer_name_age(c("sex", "time", "age  group")),
                     "age  group")
    expect_identical(infer_name_age(c("sex", "time", " age [interval]")),
                     " age [interval]")
    expect_identical(infer_name_age(c("sex", "time", "age.years")),
                     "age.years")
    expect_identical(infer_name_age(c("sex", "time", "age.year")),
                     "age.year")
})

test_that("'infer_name_age' returns NULL when not single valid answer", {
    expect_identical(infer_name_age(c("age", "sex", "age", "time")),
                     NULL)
    expect_identical(infer_name_age(c("xAge", "sex", "time")),
                     NULL)
    expect_identical(infer_name_age(c("sex", "time", "AGE_GROUPx")),
                     NULL)
    expect_identical(infer_name_age(c("sex", "time", "age--x  group")),
                     NULL)
    expect_identical(infer_name_age(c("sex", "time", " age age")),
                     NULL)
    expect_identical(infer_name_age(c("sex", "time", "age.yearss")),
                     NULL)
})


## 'make_const' --------------------------------------------------------------- 

test_that("'make_const' works with valid inputs", {
    ans_obtained <- make_const(list(a = N(), b = RW(), c = N()))
    ans_expected <- rep(1.0, 3L)
    expect_identical(ans_obtained, ans_expected)
    expect_true(is.double(ans_expected))
})


## 'make_hyper' ---------------------------------------------------------------

test_that("'make_hyper' works with valid inputs", {
    ans_obtained <- make_hyper(list(a = N(), b = RW(), c = N()))
    ans_expected <- rep(0, 3L)
    expect_identical(ans_obtained, ans_expected)
    expect_true(is.double(ans_expected))
})


## 'make_i_prior' ---------------------------------------------------------------

test_that("'make_i_prior' works with valid inputs", {
    ans_obtained <- make_i_prior(list(a = N(), b = RW(), c = N()))
    ans_expected <- c(a = 1L, b = 2L, c = 1L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_is_in_lik' -----------------------------------------------------------

test_that("'make_is_in_lik' works with no NAs", {
    ans_obtained <- make_is_in_lik(outcome = c(0, 1, 5),
                                   offset = c(1, 0, 3))
    ans_expected <- c(1L, 0L, 1L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_is_in_lik' works with NAs", {
    ans_obtained <- make_is_in_lik(outcome = c(0, 1, NA, 7),
                                   offset = c(1, 0, 3, NA))
    ans_expected <- c(1L, 0L, 0L, 0L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_map' -----------------------------------------------------------------

test_that("'make_map' works with no parameters treated as known", {
    priors <- list(N(), N())
    terms_par <- factor(c(1, 2, 2, 2))
    ans_obtained <- make_map(priors = priors, terms_par = terms_par)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_map' works with some parameters treated as known", {
    priors <- list(Known(-3), N(), Known(c(0.1, -0.1)), N())
    terms_par <- factor(c(1, 2, 2, 2, 3, 3, 4, 4, 4))
    ans_obtained <- make_map(priors = priors, terms_par = terms_par)
    ans_expected <- list(par = factor(c(NA, 1:3, NA, NA, 4:6)))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_matrices_par_array' --------------------------------------------------

test_that("'make_matrices_par_array' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- 1
    outcome <- xtabs(deaths ~ age + sex + time, data = data)
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_matrices_par_array(formula = formula, outcome = outcome)
    ans_expected <- list("(Intercept)" = matrix(1L, nr = 12, ncol = 1),
                         "time" = matrix(rep(c(1L, 0L, 0L, 1L), each = 6), nr = 12),
                         "age:sex" = rbind(diag(6), diag(6)))
    expect_equal(lapply(ans_obtained, as.numeric),
                 lapply(ans_expected, as.numeric))
    expect_true(all(sapply(ans_obtained, is, "sparseMatrix")))
})


## 'make_matrices_par_vec' ----------------------------------------------------

test_that("'make_matrices_par_vec' works with valid inputs", {
    data <- expand.grid(age = 0:5, time = 2000:2001, sex = 1:2)
    data$val <- 1
    data <- data[-c(3, 5), ]
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_matrices_par_vec(formula = formula, data = data)
    data_fac <- data[1:3]
    data_fac[] <- lapply(data_fac, factor)
    ans_expected <- Matrix::sparse.model.matrix(~age:sex + time,
                                                data = data_fac,
                                                contrasts.arg = lapply(data_fac,
                                                                       contrasts,
                                                                       contrast = FALSE),
                                                row.names = FALSE)
    v <- rnorm(n = ncol(ans_expected))
    expect_equal(do.call(cbind, ans_obtained) %*% v,
                 ans_expected %*% v)
    expect_identical(names(ans_obtained), c("(Intercept)", "time", "age:sex"))
})


## 'make_matrix_par_array' ----------------------------------------------------

test_that("'make_matrix_par_array' works with one-dimensional term and 3-dimensional array", {
    dim <- 2:4
    ## 1
    m <- make_matrix_par_array(dim = dim,
                         is_in_term = c(TRUE, FALSE, FALSE))
    beta <- rnorm(2)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[,,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
    ## 2
    m <- make_matrix_par_array(dim = dim,
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
    m <- make_matrix_par_array(dim = dim,
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

test_that("'make_matrix_par_array' works with two-dimensional term and 3-dimensional array", {
    dim <- 2:4
    ## 1 and 2
    m <- make_matrix_par_array(dim = dim,
                         is_in_term = c(TRUE, TRUE, FALSE))
    beta <- rnorm(6)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[,,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
    ## 1 and 3
    m <- make_matrix_par_array(dim = dim,
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
    m <- make_matrix_par_array(dim = dim,
                         is_in_term = c(FALSE, TRUE, TRUE))
    beta <- rnorm(12)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[1,,] <- beta
    ans_expected[2,,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
})

test_that("'make_matrix_par_array' works with 3-dimensional term and 3-dimensional array", {
    dim <- 2:4
    m <- make_matrix_par_array(dim = dim,
                         is_in_term = c(TRUE, TRUE, TRUE))
    beta <- rnorm(24)
    ans_obtained <- m %*% beta
    ans_expected <- array(dim = dim)
    ans_expected[,,] <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
})

test_that("'make_matrix_par_array' works with one-dimensional term and one-dimensional array", {
    dim <- 4
    m <- make_matrix_par_array(dim = dim,
                         is_in_term = TRUE)
    beta <- rnorm(4)
    ans_obtained <- m %*% beta
    ans_expected <- beta
    expect_identical(as.numeric(ans_obtained),
                     as.numeric(ans_expected))
})

test_that("'make_matrix_par_array' creates sparse matrix", {
    dim <- 2:4
    m <- make_matrix_par_array(dim = dim,
                         is_in_term = c(TRUE, FALSE, FALSE))
    expect_s4_class(m, "sparseMatrix")
    dim <- 2:4
    m <- make_matrix_par_array(dim = dim,
                         is_in_term = c(TRUE, FALSE, TRUE))
    expect_s4_class(m, "sparseMatrix")
})


## 'make_priors' --------------------------------------------------------------

test_that("'make_priors' works with valid inputs - has intercept, scale = 1", {
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_priors(formula, scale = 1)
    ans_expected <- list("(Intercept)" = N(scale = 10),
                         time = N(),
                         "age:sex" = N())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_priors' works with valid inputs - no intercept", {
    formula <- deaths ~ age:sex + time - 1
    ans_obtained <- make_priors(formula, scale = 1)
    ans_expected <- list(time = N(),
                         "age:sex" = N())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_priors' works with valid inputs - has intercept, scale = 2", {
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_priors(formula, scale = 2)
    ans_expected <- list("(Intercept)" = N(scale = 20),
                         time = N(scale = 2),
                         "age:sex" = N(scale = 2))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_priors' works with valid inputs - no intercept, scale = 2", {
    formula <- deaths ~ age:sex + time - 1
    ans_obtained <- make_priors(formula, scale = 2)
    ans_expected <- list(time = N(scale = 2),
                         "age:sex" = N(scale = 2))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_offset_array' --------------------------------------------------------

test_that("'make_offset_array' works with valid inputs - no NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    formula <- popn ~ age:sex + time
    ans_obtained <- make_offset_array(formula = formula,
                                      vname_offset = "popn",
                                      data = data)
    ans_expected <- xtabs(popn ~ age + sex + time, data = data)
    ans_expected <- array(1 * ans_expected,
                          dim = dim(ans_expected),
                          dimnames = dimnames(ans_expected))
    expect_identical(ans_obtained, ans_expected)
    expect_identical(names(dimnames(ans_obtained)), c("age", "sex", "time"))
})

test_that("'make_offset_array' works with valid inputs - has NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$popn <- seq_len(nrow(data))
    data$popn[3] <- NA
    formula <- popn ~ age:sex + time
    ans_obtained <- make_offset_array(formula = formula,
                                      vname_offset = "popn",
                                      data = data)
    ans_expected <- xtabs(popn ~ age + sex + time, data = data)
    ans_expected[3] <- NA
    ans_expected <- array(1 * ans_expected,
                          dim = dim(ans_expected),
                          dimnames = dimnames(ans_expected))
    expect_identical(ans_obtained, ans_expected)
    expect_identical(names(dimnames(ans_obtained)), c("age", "sex", "time"))
})


## 'make_offset_vec' ----------------------------------------------------------

test_that("'make_offset_vec' works with valid inputs - no NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$wt <- seq_len(nrow(data))
    ans_obtained <- make_offset_vec(vname_offset = "wt",
                                    data = data)
    ans_expected <- as.double(data$wt)
    ans_expected <- ans_expected / mean(ans_expected)
    expect_identical(ans_obtained, ans_expected)
    expect_equal(mean(ans_obtained), 1)
})

test_that("'make_offset_vec' works with valid inputs - has NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$wt <- seq_len(nrow(data))
    data$wt[3] <- NA
    ans_obtained <- make_offset_vec(vname_offset = "wt",
                                    data = data)
    ans_expected <- xtabs(wt ~ age + sex + time, data = data)
    ans_expected[3] <- NA
    ans_expected <- data$wt
    ans_expected <- ans_expected / mean(ans_expected, na.rm = TRUE)
    expect_identical(ans_obtained, ans_expected)
    expect_equal(mean(ans_obtained, na.rm = TRUE), 1)
})


## 'make_offset_ones_array' ---------------------------------------------------

test_that("'make_offset_ones_array' works with valid inputs, all combin present in data", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- 1:12
    formula = deaths ~ age + sex + time
    ans_obtained <- make_offset_ones_array(formula = formula, data = data)
    ans_expected <- array(1.0, dim = c(3, 2, 2), dimnames = list(age = 0:2, sex = 1:2, time = 2000:2001))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_ones_array' works with valid inputs, some combin not present in data", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- 1:12
    data <- data[-c(3, 11), ]
    formula = deaths ~ age + sex + time
    ans_obtained <- make_offset_ones_array(formula = formula, data = data)
    ans_expected <- array(1.0, dim = c(3, 2, 2), dimnames = list(age = 0:2, sex = 1:2, time = 2000:2001))
    ans_expected[c(3, 11)] <- 0
    expect_identical(ans_obtained, ans_expected)
})


## 'make_offset_ones_vec' -----------------------------------------------------

test_that("'make_offset_ones_vec' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- 1:12
    ans_obtained <- make_offset_ones_vec(data)
    ans_expected <- rep(1.0, times = 12)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_outcome_array' -------------------------------------------------------

test_that("'make_outcome_array' works with valid inputs, no NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- seq_len(nrow(data))
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_outcome_array(formula = formula,
                                       data = data)
    ans_expected <- xtabs(deaths ~ age + sex + time, data = data)
    ans_expected <- array(1 * ans_expected,
                          dim = dim(ans_expected),
                          dimnames = dimnames(ans_expected))
    expect_identical(ans_obtained, ans_expected)
    expect_identical(names(dimnames(ans_obtained)), c("age", "sex", "time"))
})

test_that("'make_outcome_array' works with valid inputs, has NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- seq_len(nrow(data))
    data$deaths[3] <- NA
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_outcome_array(formula = formula,
                                       data = data)
    ans_expected <- xtabs(deaths ~ age + sex + time, data = data)
    ans_expected[3] <- NA
    ans_expected <- array(1 * ans_expected,
                          dim = dim(ans_expected),
                          dimnames = dimnames(ans_expected))
    expect_identical(ans_obtained, ans_expected)
    expect_identical(names(dimnames(ans_obtained)), c("age", "sex", "time"))
})


## 'make_outcome_vec' ---------------------------------------------------------

test_that("'make_outcome_vec' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- seq_len(nrow(data))
    data$deaths[3] <- NA
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_outcome_vec(formula = formula,
                                     data = data)
    ans_expected <- as.double(data$deaths)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_par' -----------------------------------------------------------------

test_that("'make_par' works with valid inputs", {
    priors <- list(Known(-3), N(), Known(c(0.1, -0.1)), N())
    terms_par <- factor(c(1, 2, 2, 2, 3, 3, 4, 4, 4))
    ans_obtained <- make_par(priors = priors, terms_par = terms_par)
    ans_expected <- c(-3, 0, 0, 0, 0.1, -0.1, 0, 0, 0)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_scale_outcome' -------------------------------------------------------

test_that("'make_scale_outcome' works with valid inputs, no NA", {
    outcome <- c(1:10, NA)
    ans_obtained <- make_scale_outcome(outcome)
    ans_expected <- signif(sd(outcome[-11]), 2)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_scale_outcome' works with valid inputs, all NA", {
    expect_identical(make_scale_outcome(c(NA, NA)),
                     NA_real_)
})

test_that("'make_scale_outcome' works with valid inputs, one non-NA", {
    expect_identical(make_scale_outcome(c(NA, 3L, NA)),
                     3.0)
})


## 'make_terms_const' ---------------------------------------------------------

test_that("'make_terms_const' works with valid inputs", {
    ans_obtained <- make_terms_const(list(a = N(), b = RW(), c = Known(1:3), d = N()))
    ans_expected <- factor(c("a", "b", "d"), levels = c("a", "b", "c", "d"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_hyper' ---------------------------------------------------------

test_that("'make_terms_hyper' works with valid inputs", {
    ans_obtained <- make_terms_hyper(list(a = N(), b = RW(), c = N()))
    ans_expected <- factor(c("a", "b", "c"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_par' -----------------------------------------------------------

test_that("'make_terms_par' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- 1
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_terms_par(formula = formula, data = data)
    ans_expected <- factor(rep(c("(Intercept)", "time", "age:sex"),
                               times = c(1, 2, 6)),
                           levels = c("(Intercept)", "time", "age:sex"))
    expect_identical(ans_obtained, ans_expected)
})
