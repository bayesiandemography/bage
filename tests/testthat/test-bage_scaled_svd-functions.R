
test_that("'get_matrix_or_offset_svd' works with age main effect, type is total, matrix", {
    scaled_svd <- sim_scaled_svd()
    ans_obtained <- get_matrix_or_offset_svd(scaled_svd,
                                             levels_effect = c("0-4", "5-9"),
                                             indep = NULL,
                                             agesex = "age",
                                             get_matrix = TRUE,
                                             n_comp = 3L)
    ans_expected <- Matrix::sparseMatrix(i = rep(1:2, 3),
                                         j = rep(1:3, each = 2),
                                         x = 1,
                                         dimnames = list(c("0-4", "5-9"), NULL))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' works with age main effect, type is total, offset", {
    scaled_svd <- sim_scaled_svd()
    ans_obtained <- get_matrix_or_offset_svd(scaled_svd,
                                             levels_effect = c("0-4", "5-9"),
                                             indep = NULL,
                                             agesex = "age",
                                             get_matrix = FALSE,
                                             n_comp = NULL)
    ans_expected <- c("0-4" = 1, "5-9" = 2)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' works with sex-age interaction, type is joint, offset", {
    scaled_svd <- sim_scaled_svd()
    ans_obtained <- get_matrix_or_offset_svd(scaled_svd,
                                             levels_effect = c("Male.0-4", "Male.5-9", "Female.0-4", "Female.5-9"),
                                             indep = FALSE,
                                             agesex = "sex:age",
                                             get_matrix = FALSE,
                                             n_comp = NULL)
    ans_expected <- c("Male.0-4" = 3, "Male.5-9" = 4, "Female.0-4" = 1, "Female.5-9" = 2)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' works with age-sex interaction, type is indep, matrix", {
    scaled_svd <- sim_scaled_svd()
    ans_obtained <- get_matrix_or_offset_svd(scaled_svd,
                                             levels_effect = c("0-4.Female", "0-4.Male", "5-9.Female", "5-9.Male"),
                                             indep = TRUE,
                                             agesex = "age:sex",
                                             get_matrix = TRUE,
                                             n_comp = 5)
    ans_expected <- Matrix::sparseMatrix(i = rep(1:4, 10),
                                         j = rep(1:10, each = 4),
                                         x = 3,
                                         dimnames = list(c("0-4", "0-4",
                                                           "5-9", "5-9"),
                                                         NULL))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' returns expected error with illegal combination of indep, agesex", {
    scaled_svd <- sim_scaled_svd()
    expect_error(get_matrix_or_offset_svd(scaled_svd,
                                          levels_effect = c("0-4.Female", "0-4.Male", "5-9.Female", "5-9.Male"),
                                          indep = TRUE,
                                          agesex = "other",
                                          get_matrix = TRUE,
                                          n_comp = 5),
                 "Internal error: unexpected combination of `agesex` and `indep`.")
})

test_that("'get_matrix_or_offset_svd' returns expected error with invalid labels - main effect", {
    scaled_svd <- sim_scaled_svd()
    expect_error(get_matrix_or_offset_svd(scaled_svd,
                                          levels_effect = c("0-4", "5-999"),
                                          indep = NULL,
                                          agesex = "age",
                                          get_matrix = TRUE,
                                          n_comp = 5),
                 paste("Labels for age main effect not consistent with labels for",
                       "age in `scaled_svd`."))                 
})

test_that("'get_matrix_or_offset_svd' returns expected error with invalid labels - interaction", {
    scaled_svd <- sim_scaled_svd()
    expect_error(get_matrix_or_offset_svd(scaled_svd,
                                          levels_effect = c("0-4.Female", "0-4.Male", "5-9.Female", "5-9.Wrong"),
                                          indep = TRUE,
                                          agesex = "age:sex",
                                          get_matrix = TRUE,
                                          n_comp = 5),
                 paste("Labels for interaction between age and sex/gender not consistent with labels for",
                       "age and sex/gender in `scaled_svd`."))                 
})








