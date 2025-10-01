
## All combinations of likelihood, data model, and confidentialization
## (but very small datasets and simple priors)

## No disp, no data models ----------------------------------------------------

testthat::test_that("pois no disp has exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp no exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_disp(mean = 0)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, has exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   quiet = TRUE,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, no exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_disp(mean = 0) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   quiet = TRUE,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("binom no disp", {
  set.seed(0)
  mod <- make_small_mod_binom()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("binom no disp rr3", {
  set.seed(0)
  mod <- make_small_mod_binom() |>
    set_disp(mean = 0) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod, newdata = newdata)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})


## Has disp, no data models ---------------------------------------------------

testthat::test_that("pois has disp, has exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                   include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp, no exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp rr3, use exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp rr3, no exposure", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("binom has disp", {
  set.seed(0)
  mod <- make_small_mod_binom()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("binom has disp rr3", {
  set.seed(0)
  mod <- make_small_mod_binom() |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("norm, use weights", {
  set.seed(0)
  mod <- make_small_mod_norm(use_weights = TRUE)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod, quiet = TRUE)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    quiet = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, wt = 100:104)
  augfn <- forecast(mod, newdata = newdata)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE,
                     quiet = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("norm, no weights", {
  set.seed(0)
  mod <- make_small_mod_norm(use_weights = FALSE)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
  aug <- augment(mod)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod, quiet = TRUE)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    quiet = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod, newdata = newdata)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE,
                     quiet = TRUE)
  expect_s3_class(compfn, "tbl_df")
})


## No disp, has data models ---------------------------------------------------

testthat::test_that("pois no disp - exposure datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_exposure(cv = 0.01)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3 - exposure datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_confidential_rr3() |>
    set_datamod_exposure(cv = 0.02)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, use exposure - miscount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, no exposure - miscount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_disp(mean = 0) |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, use exposure - miscount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
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
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, no exposure - miscount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
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
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    include_estimates = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, use exposure - noise datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_noise(sd = 2)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, no exposure - noise datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_disp(mean = 0) |>
    set_datamod_noise(sd = 2)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, use exposure - noise datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_confidential_rr3() |>
    set_datamod_noise(sd = 2)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, no exposure - noise datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_disp(mean = 0) |>
    set_confidential_rr3() |>
    set_datamod_noise(sd = 2)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, use exposure - noise datamod, small numbers", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2021:2025)
  data$popn <- runif(nrow(data), min = 1, max = 10)
  data$deaths <- rpois(nrow(data), lambda = 0.2 * data$popn)
  mod <- mod_pois(deaths ~ age + time,
                  data = data,
                  exposure = popn) |>
    set_disp(mean = 0) |>
    set_datamod_noise(sd = 0.5)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, no exposure - noise datamod, small numbers", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2021:2025)
  data$deaths <- rpois(nrow(data), lambda = 20)
  mod <- mod_pois(deaths ~ age + time,
                  data = data,
                  exposure = 1) |>
    set_disp(mean = 0) |>
    set_datamod_noise(sd = 0.5)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, use exposure - noise datamod, rr3, small numbers", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2021:2025)
  data$popn <- runif(nrow(data), min = 1, max = 10)
  data$deaths <- 3 * rpois(nrow(data), lambda = 0.1 * data$popn)
  mod <- mod_pois(deaths ~ age + time,
                  data = data,
                  exposure = popn) |>
    set_disp(mean = 0) |>
    set_prior(time ~ RW(s = 0.05)) |>
    set_datamod_noise(sd = 0.1) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 1:5)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, no exposure - noise datamod, rr3, small numbers", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2021:2025)
  data$deaths <- 3 * rpois(nrow(data), lambda = 20)
  mod <- mod_pois(deaths ~ age + time,
                  data = data,
                  exposure = 1) |>
    set_disp(mean = 0) |>
    set_prior(time ~ RW(s = 0.05)) |>
    set_datamod_noise(sd = 0.1) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, use exposure - overcount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, no exposure - overcount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_disp(mean = 0) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, use exposure - overcount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, no exposure - overcount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_disp(mean = 0) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, use exposure - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp, no exposure - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, use exposure - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois no disp rr3, no exposure - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("binom no disp - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_binom() |>
    set_disp(mean = 0) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("binom no disp rr3 - undercount datamod", {
  set.seed(0)
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
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})


## Has disp, has data models --------------------------------------------------

testthat::test_that("pois has disp, use exposure - miscount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp, no exposure - miscount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp rr3, use exposure - miscount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_confidential_rr3() |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp rr3, no exposure - miscount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_confidential_rr3() |>
    set_datamod_miscount(prob = data.frame(mean = 0.9, disp = 0.1),
                         rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp, use exposure - overcount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp, no exposure - overcount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp rr3, use exposure - overcount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp rr3, no exposure - overcount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_datamod_overcount(rate = data.frame(mean = 0.2, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp, use exposure - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp, no exposure - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp rr3, use exposure - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("pois has disp rr3, use exposure - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = FALSE) |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_pois")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("binom has disp - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_binom() |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1))
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("binom has disp rr3 - undercount datamod", {
  set.seed(0)
  mod <- make_small_mod_binom() |>
    set_datamod_undercount(prob = data.frame(mean = 0.9, disp = 0.1)) |>
    set_confidential_rr3()
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_binom")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod, labels = 2026:2027, output = "components")
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, popn = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("norm, use weights - noise datamod", {
  set.seed(0)
  mod <- make_small_mod_norm(use_weights = TRUE) |>
    set_datamod_noise(sd = 1)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod, quiet = TRUE)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    quiet = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, wt = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE,
                     quiet = TRUE)
  expect_s3_class(compfn, "tbl_df")
})

testthat::test_that("norm, no weights - noise datamod", {
  set.seed(0)
  mod <- make_small_mod_norm(use_weights = FALSE) |>
    set_datamod_noise(sd = 1)
  mod <- fit(mod)
  expect_s3_class(mod, "bage_mod_norm")
  aug <- augment(mod, quiet = TRUE)
  expect_s3_class(aug, "tbl_df")
  comp <- components(mod, quiet = TRUE)
  expect_s3_class(comp, "tbl_df")
  augf <- forecast(mod,
                   labels = 2026:2027,
                   include_estimates = TRUE,
                   quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compf <- forecast(mod,
                    labels = 2026:2027,
                    output = "components",
                    quiet = TRUE)
  expect_s3_class(compf, "tbl_df")
  newdata <- data.frame(age = 0:4, time = 2026, wt = 100:104)
  augfn <- forecast(mod,
                    newdata = newdata,
                    include_estimates = TRUE,
                    quiet = TRUE)
  expect_s3_class(augf, "tbl_df")
  compfn <- forecast(mod,
                     newdata = newdata,
                     output = "components",
                     include_estimates = TRUE,
                     quiet = TRUE)
  expect_s3_class(compfn, "tbl_df")
})







