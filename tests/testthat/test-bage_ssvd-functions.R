
test_that("'get_matrix_or_offset_svd' works with age main effect, type is total, matrix", {
    ssvd <- sim_ssvd()
    ans_obtained <- get_matrix_or_offset_svd(ssvd,
                                             levels_age = c("0-4", "5-9"),
                                             levels_sex = NULL,
                                             joint = NULL,
                                             agesex = "age",
                                             get_matrix = TRUE,
                                             n_comp = 3L)
    ans_expected <- Matrix::Matrix(1, nr = 2, nc = 3, 
                                         dimnames = list(c("0-4", "5-9"), NULL))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' works with age main effect, type is total, offset", {
    ssvd <- sim_ssvd()
    ans_obtained <- get_matrix_or_offset_svd(ssvd,
                                             levels_age = c("0 to 4", "5 to 9"),
                                             levels_sex = NULL,
                                             joint = NULL,
                                             agesex = "age",
                                             get_matrix = FALSE,
                                             n_comp = NULL)
    ans_expected <- c("0-4" = 1, "5-9" = 2)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' works with sex-age interaction, type is joint, offset", {
    ssvd <- sim_ssvd()
    ans_obtained <- get_matrix_or_offset_svd(ssvd,
                                             levels_age = c("0-4", "5-9"),
                                             levels_sex = c("Male", "Female"),
                                             joint = TRUE,
                                             agesex = "age:sex",
                                             get_matrix = FALSE,
                                             n_comp = NULL)
    ans_expected <- c("Male.0-4" = 3, "Male.5-9" = 4, "Female.0-4" = 1, "Female.5-9" = 2)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' works with age-sex interaction, type is indep, matrix", {
  ssvd <- sim_ssvd()
  ans_obtained <- get_matrix_or_offset_svd(ssvd,
                                           levels_age = c("0-4", "5-9"),
                                           levels_sexgender = c("Female", "Male"),
                                           joint = FALSE,
                                           agesex = "sex:age",
                                           get_matrix = TRUE,
                                           n_comp = 5)
  ans_expected <- Matrix::Matrix(3, nr = 4, nc = 10,
                                 dimnames = list(c("Female.0-4", "Male.0-4",
                                                   "Female.5-9", "Male.5-9"),
                                                 NULL))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' works with age-sex interaction, type is joint, offset, non-standard labels", {
  ssvd <- sim_ssvd()
  ans_obtained <- get_matrix_or_offset_svd(ssvd,
                                           levels_age = c("0 to 4", "5--9"),
                                           levels_sexgender = c("M", "F"),
                                           joint = FALSE,
                                           agesex = "sex:age",
                                           get_matrix = TRUE,
                                           n_comp = 5)
  ans_expected <- Matrix::Matrix(3, nr = 4, nc = 10,
                                 dimnames = list(c("Male.0-4", "Female.0-4",
                                                   "Male.5-9", "Female.5-9"),
                                                 NULL))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd' returns expected error when 'levels_age' has duplicates", {
    ssvd <- sim_ssvd()
    expect_error(get_matrix_or_offset_svd(ssvd,
                                          levels_age = c("0-4", "5-9", "0-4"),
                                          levels_sexgender = c("F", "M"),
                                          joint = FALSE,
                                          agesex = "age:sex",
                                          get_matrix = TRUE,
                                          n_comp = 5),
                 "Internal error: `levels_age` has duplicates.")
})

test_that("'get_matrix_or_offset_svd' returns expected error when 'levels_sexgender' has duplicates", {
    ssvd <- sim_ssvd()
    expect_error(get_matrix_or_offset_svd(ssvd,
                                          levels_age = c("0-4", "5-9"),
                                          levels_sexgender = c("F", "M", "M"),
                                          joint = FALSE,
                                          agesex = "age:sex",
                                          get_matrix = TRUE,
                                          n_comp = 5),
                 "Internal error: `levels_sexgender` has duplicates.")
})


test_that("'get_matrix_or_offset_svd' returns expected error with illegal value for agesex", {
    ssvd <- sim_ssvd()
    expect_error(get_matrix_or_offset_svd(ssvd,
                                          levels_age = c("0-4", "5-9"),
                                          levels_sexgender = c("F", "M"),
                                          joint = FALSE,
                                          agesex = "other",
                                          get_matrix = TRUE,
                                          n_comp = 5),
                 "Internal error: unexpected value for `agesex`.")
})

test_that("'get_matrix_or_offset_svd' returns expected error with illegal value for age label", {
    ssvd <- sim_ssvd()
    expect_error(get_matrix_or_offset_svd(ssvd,
                                          levels_age = c("0-4", "wrong"),
                                          levels_sexgender = c("F", "M"),
                                          joint = FALSE,
                                          agesex = "sex:age",
                                          get_matrix = TRUE,
                                          n_comp = 5),
                 "Unable to parse age labels when processing SVD prior.")
})

test_that("'get_matrix_or_offset_svd' returns expected error with illegal value for age label", {
    ssvd <- sim_ssvd()
    expect_error(get_matrix_or_offset_svd(ssvd,
                                          levels_age = c("0-4", "5-9"),
                                          levels_sexgender = c("F", "wrong"),
                                          joint = FALSE,
                                          agesex = "sex:age",
                                          get_matrix = TRUE,
                                          n_comp = 5),
                 "Unable to parse sex/gender labels when processing SVD prior.")
})

test_that("'get_matrix_or_offset_svd' returns expected error when can't align age labels", {
  ssvd <- sim_ssvd()
  expect_error(get_matrix_or_offset_svd(ssvd,
                                        levels_age = c("0---4", "5+"),
                                        levels_sexgender = c("F", "M"),
                                        joint = FALSE,
                                        agesex = "age:sex",
                                        get_matrix = TRUE,
                                        n_comp = 5),
               "Unable to align age labels from `data` with age labels from `ssvd`.")
})

test_that("'get_matrix_or_offset_svd' returns expected error when can't parse sex/gender labels", {
  ssvd <- sim_ssvd()
  expect_error(get_matrix_or_offset_svd(ssvd,
                                        levels_age = c("0-4", "5-9"),
                                        levels_sexgender = c("M", "FFF"),
                                        joint = FALSE,
                                        agesex = "age:sex",
                                        get_matrix = TRUE,
                                        n_comp = 5),
               "Unable to parse sex/gender labels when processing SVD prior.")
})

test_that("'get_matrix_or_offset_svd' returns expected error when can't align sex/gender labels", {
  ssvd <- sim_ssvd()
  expect_error(get_matrix_or_offset_svd(ssvd,
                                        levels_age = c("0-4", "5-9"),
                                        levels_sexgender = "M",
                                        joint = FALSE,
                                        agesex = "age:sex",
                                        get_matrix = TRUE,
                                        n_comp = 5),
               "Unable to align sex/gender labels from `data` with sex/gender labels from `ssvd`.")
})


## 'get_n_comp' -------------------------------------------------------------------

test_that("'get_n_comp' works", {
  ssvd <- sim_ssvd()
  ans_obtained <- get_n_comp(ssvd)
  ans_expected <- 10L
  expect_identical(ans_obtained, ans_expected)
})


## 'has_sexgender' ------------------------------------------------------------

test_that("'has_sexgender' works", {
  ssvd <- sim_ssvd()
  expect_true(has_sexgender(ssvd))
  ssvd$data <- ssvd$data[1,]
  expect_false(has_sexgender(ssvd))
})



