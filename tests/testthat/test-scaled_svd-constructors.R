
test_that("'scaled_svd' creates object of class 'bage_scaled_svd' with valid inputs", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"))
    data$labels_age <- rep(list(c("0-4", "5-9")), times = 4)
    data$labels_sexgender <- list(NULL, NULL, NULL, c("Female", "Male"))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    ans <- scaled_svd(data)
    expect_s3_class(ans, "bage_scaled_svd")
})

test_that("'scaled_svd' throws correct error when 'data' is not data frame", {
    expect_error(scaled_svd(NULL),
                 "`data` is not a data frame.")
})

test_that("'scaled_svd' throws correct error when names duplicated", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       "blank" = 1:4,
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    names(data)[2] <- "sexgender"
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "`data` has duplicated name: \"sexgender\".")
})

test_that("'scaled_svd' throws correct error when name invalid", {
    data <- data.frame(wrong = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "`data` does not have expected variables.")
})

test_that("'scaled_svd' throws correct error when sexgender has NA", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", NA),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "Element 4 of sex/gender variable is NA.")
})

test_that("'scaled_svd' throws correct error when matrix is not list column", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- 1:4
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = "a", "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "`matrix` is not a list column.")
})

test_that("'scaled_svd' throws correct error when offset has non-numeric", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = "a", "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "Element 2 of `offset` is non-numeric.")
})

test_that("'scaled_svd' throws correct error when offset has NA", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = NA, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "Element 2 of `offset` has NA.")
})

test_that("'scaled_svd' throws correct error when 'matrix' has non-matrices", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        1:3,
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "Element 3 of `matrix` is not a matrix.")
})

test_that("'scaled_svd' throws correct error when 'sexgender' missing categories", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "Sex/gender variable does not have category \".Joint\".")
})

test_that("'scaled_svd' throws correct error when 'matrix', 'offset' inconsistent", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22, "10-14" = 23),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "`matrix` and `offset` not consistent")
})

test_that("'scaled_svd' throws correct error when matrices not all 10 columns", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 9, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "Element 4 of `matrix` does not have 10 columns.")
})

test_that("'scaled_svd' throws correct error when matrix does not have rownames", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 31, "5-9" = 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "Element 1 of `matrix` does not have rownames.")
})

test_that("'scaled_svd' throws correct error when offset does not have names", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c(31, 32),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "Element 3 of `offset` does not have names.")
})

test_that("'scaled_svd' throws correct error when rownames, names different", {
    data <- data.frame(sexgender = c("Female", "Male", ".Total", ".Joint"),
                       labels_age = rep(list(c("0-4", "5-9")), times = 4),
                       labels_sexgender = list(NULL, NULL, NULL, c("Female", "Male")))
    data$matrix <- list(matrix(1, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(5, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(11, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)),
                        matrix(15, nr = 2, nc = 10, dimnames = list(c("0-4", "5-9"), NULL)))
    data$offset <- list(c("0-4" = 11, "5-9" = 12),
                        c("0-4" = 21, "5-9" = 22),
                        c("0-4" = 21, "15-19" = 22),
                        c("0-4" = 41, "5-9" = 42))
    expect_error(scaled_svd(data),
                 "`matrix` and `offset` not consistent.")
})


















    
