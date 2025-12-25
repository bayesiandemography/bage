
## All priors (but very small datasets)

testthat::skip_on_cran()

testthat::test_that("'ar main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ AR()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'ar interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ AR()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'ar1 main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ AR1()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'ar1 interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ AR1()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

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
  expect_s3_class(rep, "tbl_df")
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
  expect_s3_class(rep, "tbl_df")
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
  expect_s3_class(rep, "tbl_df")
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
  expect_s3_class(rep, "tbl_df")
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
  expect_s3_class(rep, "tbl_df")
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
  expect_s3_class(rep, "tbl_df")
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
  expect_s3_class(rep, "tbl_df")
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
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'known main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ Known(c(0.1, 0, 0.1, 0, 0.1))) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'known interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ Known(values = rnorm(25))) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'lin main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ Lin()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'lin interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ Lin()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'linar main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ Lin_AR()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'linar interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ Lin_AR()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'linex main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ Lin(s = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'linex interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ Lin(s = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'norm main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ N()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'norm interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ N()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'normfixed main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ NFix()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'normfixed interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ NFix()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwrandom main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ RW()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwrandom interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ RW()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwrandomseasfix main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ RW_Seas(n_seas = 2, s_seas = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwrandomseasfix interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ RW_Seas(n_seas = 2, s_seas = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwrandomseasrandom main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ RW_Seas(n_seas = 2)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwrandomseasrandom interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ RW_Seas(n_seas = 2)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwzero main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ RW(sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwzero interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ RW(sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwzeroseasfix main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ RW_Seas(sd = 0, n_seas = 2, s_seas = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwzeroseasfix interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ RW_Seas(sd = 0, n_seas = 2, s_seas = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwzeroseasrandom main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ RW_Seas(sd = 0, n_seas = 2)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rwzeroseasrandom interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ RW_Seas(sd = 0, n_seas = 2)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})




testthat::test_that("'rw2random main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ RW2()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rw2random interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ RW2()) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rw2zero main effect", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE) |>
    set_prior(time ~ RW2(sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})

testthat::test_that("'rw2zero interaction", {
  set.seed(0)
  mod <- make_small_mod_pois(use_exposure = TRUE, use_interaction = TRUE) |>
    set_prior(age:time ~ RW2(sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_pois")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(rep, "tbl_df")
})








testthat::test_that("'svd_drwrandom", {
  set.seed(0)
  data <- expand.grid(age = 5:14, sex = c("Female", "Male"), time = 2001:2010)
  data$popn <- rpois(n = nrow(data), lambda = 100) + 10
  data$attendance <- rbinom(n = nrow(data), size = data$popn, prob = 0.8)
  mod <- mod_binom(attendance ~ age * time + sex,
                   data = data,
                   size = popn) |>
    set_prior(age:time ~ SVD_DRW(CSA)) |>
    fit()
  expect_s3_class(mod, "bage_mod_binom")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'svd_drwzero", {
  set.seed(0)
  data <- expand.grid(age = 5:14, sex = c("Female", "Male"), time = 2001:2010)
  data$popn <- rpois(n = nrow(data), lambda = 100) + 10
  data$attendance <- rbinom(n = nrow(data), size = data$popn, prob = 0.8)
  mod <- mod_binom(attendance ~ age * time + sex,
                   data = data,
                   size = popn) |>
    set_prior(age:time ~ SVD_DRW(CSA, sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_binom")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'svd_drw2random", {
  set.seed(0)
  data <- expand.grid(age = 5:14, sex = c("Female", "Male"), time = 2001:2010)
  data$popn <- rpois(n = nrow(data), lambda = 100) + 10
  data$attendance <- rbinom(n = nrow(data), size = data$popn, prob = 0.8)
  mod <- mod_binom(attendance ~ age * time + sex,
                   data = data,
                   size = popn) |>
    set_prior(age:time ~ SVD_DRW2(CSA)) |>
    fit()
  expect_s3_class(mod, "bage_mod_binom")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

testthat::test_that("'svd_drw2zero", {
  set.seed(0)
  data <- expand.grid(age = 5:14, sex = c("Female", "Male"), time = 2001:2010)
  data$popn <- rpois(n = nrow(data), lambda = 100) + 10
  data$attendance <- rbinom(n = nrow(data), size = data$popn, prob = 0.8)
  mod <- mod_binom(attendance ~ age * time + sex,
                   data = data,
                   size = popn) |>
    set_prior(age:time ~ SVD_DRW2(CSA, sd = 0)) |>
    fit()
  expect_s3_class(mod, "bage_mod_binom")
  comp <- components(mod)
  expect_s3_class(comp, "tbl_df")
  fc <- forecast(mod, labels = 2026:2027)
  expect_s3_class(fc, "tbl_df")
  rep <- replicate_data(mod, n = 2)
  expect_s3_class(fc, "tbl_df")
})

