
## 'get_from_comp_coef' -------------------------------------------------------

test_that("'get_from_comp_coef' works", {
  components <- tibble::tibble(
    term = "age",
    component = c(rep("effect", 5), rep("hyper", 2)),
    level = c(0:4, "sd", "coef"),
    .fitted = rvec::runif_rvec(n = 7)
  )
  expect_identical(get_from_comp_coef(components = components,
                                      term = "age"),
                   components$.fitted[[7]])
  expect_error(get_from_comp_coef(components = components,
                                  term = "sex"),
               "Internal error")
})

## 'get_from_comp_effect' -----------------------------------------------------

test_that("'get_from_comp_effect' works", {
  components <- tibble::tibble(
    term = "age",
    component = c(rep("effect", 5), rep("hyper", 2)),
    level = c(0:4, "sd", "coef"),
    .fitted = rvec::runif_rvec(n = 7)
  )
  expect_identical(get_from_comp_effect(components = components,
                                        term = "age"),
                   components$.fitted[1:5])
  expect_error(get_from_comp_effect(components = components,
                                    term = "sex"),
               "Internal error")
})

## 'get_from_comp_error' -----------------------------------------------------

test_that("'get_from_comp_error' works", {
  components <- tibble::tibble(
    term = "age",
    component = c(rep("error", 5), rep("hyper", 2)),
    level = c(0:4, "sd", "coef"),
    .fitted = rvec::runif_rvec(n = 7)
  )
  expect_identical(get_from_comp_error(components = components,
                                       term = "age"),
                   components$.fitted[1:5])
  expect_error(get_from_comp_error(components = components,
                                   term = "sex"),
               "Internal error")
})

## 'get_from_comp_sd' ---------------------------------------------------------

test_that("'get_from_comp_sd' works", {
  components <- tibble::tibble(
    term = "age",
    component = c(rep("effect", 5), rep("hyper", 2)),
    level = c(0:4, "sd", "sd_seas"),
    .fitted = rvec::runif_rvec(n = 7)
  )
  expect_identical(get_from_comp_sd(components = components,
                                         term = "age"),
                   components$.fitted[[6]])
  expect_error(get_from_comp_sd(components = components,
                                     term = "sex"),
               "Internal error")
})

## 'get_from_comp_sd_ar' ----------------------------------------------------

test_that("'get_from_comp_sd_ar' works", {
  components <- tibble::tibble(
    term = "age",
    component = c(rep("effect", 5), rep("hyper", 3)),
    level = c(0:4, "sd", "sd_ar", "sd_rw"),
    .fitted = rvec::runif_rvec(n = 8)
  )
  expect_identical(get_from_comp_sd_ar(components = components,
                                       term = "age"),
                   components$.fitted[[7]])
  expect_error(get_from_comp_sd_ar(components = components,
                                   term = "sex"),
               "Internal error")
})

## 'get_from_comp_sd_rw' ----------------------------------------------------

test_that("'get_from_comp_sd_rw' works", {
  components <- tibble::tibble(
    term = "age",
    component = c(rep("effect", 5), rep("hyper", 3)),
    level = c(0:4, "sd", "sd_ar", "sd_rw"),
    .fitted = rvec::runif_rvec(n = 8)
  )
  expect_identical(get_from_comp_sd_rw(components = components,
                                       term = "age"),
                   components$.fitted[[8]])
  expect_error(get_from_comp_sd_rw(components = components,
                                   term = "sex"),
               "Internal error")
})

## 'get_from_comp_sd_seas' ----------------------------------------------------

test_that("'get_from_comp_sd_seas' works", {
  components <- tibble::tibble(
    term = "age",
    component = c(rep("effect", 5), rep("hyper", 2)),
    level = c(0:4, "sd", "sd_seas"),
    .fitted = rvec::runif_rvec(n = 7)
  )
  expect_identical(get_from_comp_sd_seas(components = components,
                                         term = "age"),
                   components$.fitted[[7]])
  expect_error(get_from_comp_sd_seas(components = components,
                                     term = "sex"),
               "Internal error")
})

test_that("'get_from_comp_season' works", {
  components <- tibble::tibble(
    term = "time",
    component = c(rep("effect", 5), rep("season", 5), rep("trend", 5)),
    level = rep(2001:2005, times = 3),
    .fitted = rvec::runif_rvec(n = 15)
  )
  expect_identical(get_from_comp_season(components = components,
                                        term = "time"),
                   components$.fitted[6:10])
  expect_error(get_from_comp_season(components = components,
                                    term = "sex"),
               "Internal error")
})

test_that("'get_from_comp_slope' works", {
  components <- tibble::tibble(
    term = "time",
    component = c(rep("effect", 5), rep("hyper", 2)),
    level = c(2001:2005, "sd", "slope"),
    .fitted = rvec::runif_rvec(n = 7)
  )
  expect_identical(get_from_comp_slope(components = components,
                                       term = "time"),
                   components$.fitted[7])
  expect_error(get_from_comp_slope(components = components,
                                   term = "sex"),
               "Internal error")
})

test_that("'get_from_comp_svd' works", {
  components <- tibble::tibble(
    term = "age",
    component = c(rep("effect", 5), rep("svd", 3)),
    level = c(0:4, 1, 2, 3),
    .fitted = rvec::runif_rvec(n = 8)
  )
  expect_identical(get_from_comp_svd(components = components,
                                     term = "age"),
                   components$.fitted[6:8])
  expect_error(get_from_comp_svd(components = components,
                                 term = "sex"),
               "Internal error")
})

test_that("'get_from_comp_trend' works", {
  components <- tibble::tibble(
    term = "time",
    component = c(rep("effect", 5), rep("season", 5), rep("trend", 5)),
    level = rep(2001:2005, times = 3),
    .fitted = rvec::runif_rvec(n = 15)
  )
  expect_identical(get_from_comp_trend(components = components,
                                        term = "time"),
                   components$.fitted[11:15])
  expect_error(get_from_comp_trend(components = components,
                                    term = "sex"),
               "Internal error")
})






