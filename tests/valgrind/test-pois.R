suppressPackageStartupMessages(library(bage))

testthat::test_that("pois no disp has exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
})

testthat::test_that("pois has disp rr3, no exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
})

testthat::test_that("pois no disp - exposure datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_exposure(cv = 0.01)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
})
