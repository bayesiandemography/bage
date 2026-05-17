
suppressPackageStartupMessages(library(bage))

testthat::test_that("binom no disp rr3", {
  set.seed(0)
  mod <- make_small_mod_binom() |>
    set_disp(mean = 0) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
})

testthat::test_that("binom has disp", {
  set.seed(0)
  mod <- make_small_mod_binom()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
})

testthat::test_that("binom no disp - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_binom() |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
})
