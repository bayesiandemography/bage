
test_that("'scaled_svd' creates object of class 'bage_scaled_svd' with valid inputs", {
    s <- sim_scaled_svd()
    expect_s3_class(s, "bage_scaled_svd")
})

test_that("'scaled_svd' throws correct error when 'data' is not data frame", {
    expect_error(scaled_svd(NULL),
                 "`data` is not a data frame.")
})

test_that("'scaled_svd' throws correct error when names duplicated", {
    data <- sim_scaled_svd()$data
    data$type2 <- data$type
    names(data)[length(names(data))] <- "type"
    expect_error(scaled_svd(data),
                 "`data` has duplicated name: \"type\".")
})

test_that("'scaled_svd' throws correct error when name invalid", {
    data <- sim_scaled_svd()$data
    names(data)[[1L]] <- "wrong"
    expect_error(scaled_svd(data),
                 "`data` does not have expected variables.")
})

test_that("'scaled_svd' throws correct error when type has NA", {
    data <- sim_scaled_svd()$data
    data$type[3] <- NA
    expect_error(scaled_svd(data),
                 "Element 3 of `type` is NA.")
})

test_that("'scaled_svd' throws correct error when type has invalid category", {
    data <- sim_scaled_svd()$data
    data$type[3] <- "wrong"
    expect_error(scaled_svd(data),
                 "`type` has invalid set of categories.")
})

test_that("'scaled_svd' throws correct error when matrix is not list column", {
    data <- sim_scaled_svd()$data
    data$matrix <- 1:3
    expect_error(scaled_svd(data),
                 "`matrix` is not a list column.")
})

test_that("'scaled_svd' throws correct error when offset has NA", {
    data <- sim_scaled_svd()$data
    data$offset[[3]][[1]] <- NA
    expect_error(scaled_svd(data),
                 "Element 3 of `offset` has NA.")
})

test_that("'scaled_svd' throws correct error when 'labels_sexgender' NULL in the wrong place", {
    data <- sim_scaled_svd()$data
    data$labels_sexgender[2] <- list(NULL)
    expect_error(scaled_svd(data),
                 "`type` and `labels_sexgender` are inconsistent.")
})

test_that("'scaled_svd' throws correct error when 'labels_sexgender' and 'labels_age' inconsistent", {
    data <- sim_scaled_svd()$data
    data$labels_sexgender[[2]] <- data$labels_sexgender[[2]][1:2]
    expect_error(scaled_svd(data),
                 "`labels_age` and `labels_sexgender` are inconsistent.")
})

test_that("'scaled_svd' throws correct error when 'labels_age' and 'offset' inconsistent", {
    data <- sim_scaled_svd()$data
    data$offset[[2]] <- data$offset[[2]][1:2]
    expect_error(scaled_svd(data),
                 "`labels_age` and `offset` are inconsistent.")
})

test_that("'scaled_svd' throws correct error when 'matrix' has non-sparse-matrices", {
    data <- sim_scaled_svd()$data
    data$matrix[[2]] <- as.matrix(data$matrix[[2]])
    expect_error(scaled_svd(data),
                 "Element 2 of `matrix` is not a sparse matrix.")
})

test_that("'scaled_svd' throws correct error when offset has non-numeric", {
    data <- sim_scaled_svd()$data
    data$offset[[3]][[1]] <- "a"
    expect_error(scaled_svd(data),
                 "Element 3 of `offset` is non-numeric.")
})

test_that("'scaled_svd' throws correct error when 'matrix', 'offset' inconsistent", {
    data <- sim_scaled_svd()$data
    data$matrix[[3]] <- data$matrix[[3]][-1,]
    expect_error(scaled_svd(data),
                 "`matrix` and `offset` not consistent")
})

test_that("'scaled_svd' throws correct error when matrices has unexpected number of columns", {
    data <- sim_scaled_svd()$data
    data$matrix[[3]] <- data$matrix[[2]]
    expect_error(scaled_svd(data),
                 "Element 3 of `matrix` has wrong number of columns.")
})

test_that("'scaled_svd' throws correct error when matrix does not have rownames", {
    data <- sim_scaled_svd()$data
    rownames(data$matrix[[1]]) <- NULL
    expect_error(scaled_svd(data),
                 "Element 1 of `matrix` does not have rownames.")
})

test_that("'scaled_svd' throws correct error when offset does not have names", {
    data <- sim_scaled_svd()$data
    names(data$offset[[3]]) <- NULL
    expect_error(scaled_svd(data),
                 "Element 3 of `offset` does not have names.")
})

test_that("'scaled_svd' throws correct error when rownames, names different", {
    data <- sim_scaled_svd()$data
    names(data$offset[[3]]) <- c("a", "b", "a", "b")
    expect_error(scaled_svd(data),
                 "`matrix` and `offset` not consistent.")
})


















    
