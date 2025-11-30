
## All combinations of likelihood, data model, and confidentialization
## (but very small datasets and simple priors)

testthat::skip_on_cran()

testthat::test_that("'drwrandom main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ DRW()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'drwrandom interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ DRW()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'drwzero main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ DRW(sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'drwzero interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ DRW(sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'drw2random main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ DRW2()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'drw2random interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ DRW2()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'drw2zero main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ DRW2(sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'drw2zero interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ DRW2(sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

