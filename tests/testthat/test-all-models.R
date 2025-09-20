
## All combinations of likelihood, data model, and confidentialization
## (but very small datasets and simple priors)


## No disp, no data models ----------------------------------------------------

testthat::test_that("pois no disp", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp rr3", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("binom no disp", {
  mod <- make_small_mod_binom()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("binom no disp rr3", {
  mod <- make_small_mod_binom() |>
    set_disp(mean = 0) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})


## Has disp, no data models ---------------------------------------------------

testthat::test_that("pois has disp", {
  mod <- make_small_mod_pois()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois has disp rr3", {
  mod <- make_small_mod_pois() |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("binom has disp", {
  mod <- make_small_mod_binom()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("binom has disp rr3", {
  mod <- make_small_mod_binom() |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("norm", {
  mod <- make_small_mod_norm()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod, quiet = TRUE)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    quiet = TRUE)
  expect_s3_class(compf, "tbl_df")
})


## No disp, has data models ---------------------------------------------------

testthat::test_that("pois no disp - exposure datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_datamod_exposure(disp = 0.2)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp rr3 - exposure datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_confidential_rr3() |>
    set_datamod_exposure(disp = 0.2)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp - miscount datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp rr3 - miscount datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_confidential_rr3() |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp - noise datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_datamod_noise(sd = 2)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp rr3 - noise datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_confidential_rr3() |>
    set_datamod_noise(sd = 2)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp - overcount datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp rr3 - overcount datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp - undercount datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois no disp rr3 - undercount datamod", {
  mod <- make_small_mod_pois() |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("binom no disp - undercount datamod", {
  mod <- make_small_mod_binom() |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("binom no disp rr3 - undercount datamod", {
  mod <- make_small_mod_binom() |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})


## Has disp, has data models --------------------------------------------------

testthat::test_that("pois has disp - miscount datamod", {
  mod <- make_small_mod_pois() |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois has disp rr3 - miscount datamod", {
  mod <- make_small_mod_pois() |>
    set_confidential_rr3() |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois has disp - overcount datamod", {
  mod <- make_small_mod_pois() |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois has disp rr3 - overcount datamod", {
  mod <- make_small_mod_pois() |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois has disp - undercount datamod", {
  mod <- make_small_mod_pois() |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("pois has disp rr3 - undercount datamod", {
  mod <- make_small_mod_pois() |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("binom has disp - undercount datamod", {
  mod <- make_small_mod_binom() |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("binom has disp rr3 - undercount datamod", {
  mod <- make_small_mod_binom() |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
})

testthat::test_that("norm - noise datamod", {
  mod <- make_small_mod_norm() |>
    set_datamod_noise(sd = 1)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod, quiet = TRUE)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod, labels = 2026:2027)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components", quiet = TRUE)
  expect_s3_class(compf, "tbl_df")
})







