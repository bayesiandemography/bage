suppressPackageStartupMessages(library(bage))

testthat::test_that("norm, use weights", {
  set.seed(0)
  mod <- make_small_mod_norm(use_weights = TRUE)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
})

testthat::test_that("norm, no weights", {
  set.seed(0)
  mod <- make_small_mod_norm(use_weights = FALSE)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
})

testthat::test_that("norm, use weights - noise datamod", {
  set.seed(0)
  mod <- make_small_mod_norm(use_weights = TRUE) |>
    set_datamod_noise(sd = 1)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
})
