

## User-visible constructors --------------------------------------------------

test_that("'AR' works with valid inputs", {
  expect_identical(AR(n = 3),
                   new_bage_prior_ar(n = 3L,
                                     min = -1,
                                     max = 1,
                                     scale = 1,
                                     nm = "AR"))
  expect_identical(AR(n = 1, s = 0.01),
                   new_bage_prior_ar(n = 1L,
                                     min = -1,
                                     max = 1,
                                     scale = 0.01,
                                     nm = "AR"))
})

test_that("'AR1' works with valid inputs", {
  expect_identical(AR1(),
                   new_bage_prior_ar(n = 1L,
                                     min = 0.8,
                                     max = 0.98,
                                     scale = 1,
                                     nm = "AR1"))
})

test_that("'AR1' throws current error when min >= max", {
    expect_error(AR1(min = 0.8, max = 0.8),
                 "`max` is less than or equal to `min`")
})

test_that("'compose_time' works with valid inputs", {
  ans_obtained <- compose_time(error = N(), trend = ELin(s = 0.3))
  ans_expected <- new_bage_prior_compose(priors = list(trend = ELin(s = 0.3),
                                                       error = N()),
                                         along = NULL,
                                         nm = "compose_time")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'compose_time' throws correct error when 'trend' non-prior", {
  expect_error(compose_time(trend = "wrong"),
               "`trend` has class <character>")
})

test_that("'compose_time' throws correct error when 'trend' invalid prior", {
  expect_error(compose_time(trend = Seas(n = 3)),
               "`Seas\\(n=3\\)` prior cannot be used for `trend`")
})

test_that("'compose_time' throws correct error when 'cyclical' non-prior", {
  expect_error(compose_time(trend = RW(), seasonal = "wrong"),
               "`seasonal` has class <character>")
})

test_that("'compose_time' throws correct error when 'cyclical' invalid prior", {
  expect_error(compose_time(trend = Lin(), cyclical = N()),
               "`N\\(\\)` prior cannot be used for `cyclical`")
})

test_that("'compose_time' throws correct error when not enough priors specified", {
  expect_error(compose_time(trend = Lin()),
               "Not enough priors specified.")
})  

test_that("'EAR' works with valid inputs", {
  expect_identical(EAR(n = 3),
                   new_bage_prior_ear(n = 3L,
                                      min = -1,
                                      max = 1,
                                      scale = 1,
                                      along = NULL,
                                      nm = "EAR"))
  expect_identical(EAR(n = 1, s = 0.01, along = "time"),
                   new_bage_prior_ear(n = 1L,
                                      min = -1,
                                      max = 1,
                                      scale = 0.01,
                                      along = "time",
                                      nm = "EAR"))
})

test_that("'EAR1' works with valid inputs", {
  expect_identical(EAR1(),
                   new_bage_prior_ear(n = 1L,
                                      min = 0.8,
                                      max = 0.98,
                                      scale = 1,
                                      along = NULL,
                                      nm = "EAR1"))
  expect_identical(EAR1(s = 0.01, min = -1, max = 1),
                   new_bage_prior_ear(n = 1L,
                                      min = -1,
                                      max = 1,
                                      scale = 0.01,
                                      along = NULL,
                                      nm = "EAR1"))
})

test_that("'ELin' works with valid inputs", {
  expect_identical(ELin(), new_bage_prior_elin(scale = 1, sd_slope = 1,
                                               mscale = 1, along = NULL))
  expect_identical(ELin(s = 0.3, sd = 0.1, along = "reg"),
                   new_bage_prior_elin(scale = 0.3, sd_slope = 0.1,
                                       mscale = 1, along = "reg"))
})

test_that("'ERW' works with valid inputs", {
  expect_identical(ERW(), new_bage_prior_erw(scale = 1, along = NULL))
  expect_identical(ERW(s = 0.3, along = "reg"),
                   new_bage_prior_erw(scale = 0.3, along = "reg"))
})

test_that("'ESeas' works with valid inputs", {
  expect_identical(ESeas(n = 4),
                   new_bage_prior_eseas(n = 4L,
                                        scale = 1, 
                                        along = NULL))
  expect_identical(ESeas(s = 0.3, n = 2, along = "reg"),
                   new_bage_prior_eseas(n = 2L,
                                        scale = 0.3,
                                        along = "reg"))
})

test_that("'Known' works with valid inputs", {
    expect_identical(Known(values = 1:3),
                     new_bage_prior_known(values = as.double(1:3)))
})

test_that("'Lin' works with valid inputs", {
    expect_identical(Lin(), new_bage_prior_lin(scale = 1, sd_slope = 1))
    expect_identical(Lin(s = 0.3, sd = 0.1), new_bage_prior_lin(scale = 0.3, sd_slope = 0.1))
})

test_that("'N' works with valid inputs", {
    expect_identical(N(), new_bage_prior_norm(scale = 1))
})

test_that("'NFix' works with valid inputs", {
    expect_identical(NFix(), new_bage_prior_normfixed(sd = 1))
})

test_that("'RW' works with valid inputs", {
    expect_identical(RW(), new_bage_prior_rw(scale = 1))
})

test_that("'RW2' works with valid inputs", {
    expect_identical(RW2(), new_bage_prior_rw2(scale = 1, sd_slope = 1))
})

test_that("'Seas' works with valid inputs", {
    expect_identical(Seas(n = 2), new_bage_prior_seas(n = 2L, scale = 1))
    expect_identical(Seas(n = 12L, s = 0.1), new_bage_prior_seas(n = 12L, scale = 0.1))
})

test_that("'Sp' works with valid inputs", {
    expect_identical(Sp(), new_bage_prior_spline(n = NULL, scale = 1, sd_slope = 1))
    expect_identical(Sp(n = 6, s = 3, sd = 0.2),
                     new_bage_prior_spline(n = 6L, scale = 3, sd_slope = 0.2))
})

test_that("'SVD' works with valid inputs", {
    expect_identical(SVD(HMD),
                     new_bage_prior_svd(HMD,
                                        nm_ssvd = "HMD",
                                        joint = NULL,
                                        n = 5L))
    expect_identical(SVD(HMD, n = 3),
                     new_bage_prior_svd(HMD,
                                        nm_ssvd = "HMD",
                                        joint = NULL,
                                        n = 3L))
})

test_that("'SVDS' works with valid inputs", {
    expect_identical(SVDS(HMD),
                     new_bage_prior_svd(HMD,
                                        nm_ssvd = "HMD",
                                        joint = FALSE,
                                        n = 5L))
    expect_identical(SVDS(HMD, joint = TRUE, n = 3),
                     new_bage_prior_svd(HMD,
                                        nm_ssvd = "HMD",
                                        joint = TRUE,
                                        n = 3L))
})

test_that("'ESVD' works with valid inputs", {
  expect_identical(ESVD(HMD),
                   new_bage_prior_esvd(HMD,
                                       nm_ssvd = "HMD",
                                       joint = NULL,
                                       n = 5L))
  expect_identical(ESVD(HMD, n = 3),
                   new_bage_prior_esvd(HMD,
                                       nm_ssvd = "HMD",
                                       joint = NULL,
                                       n = 3L))
})

test_that("'ESVDS' works with valid inputs", {
  expect_identical(ESVDS(HMD),
                   new_bage_prior_esvd(HMD,
                                       nm_ssvd = "HMD",
                                       joint = FALSE,
                                       n = 5L))
  expect_identical(ESVDS(HMD, joint = TRUE, n = 3),
                   new_bage_prior_esvd(HMD,
                                       nm_ssvd = "HMD",
                                       joint = TRUE,
                                       n = 3L))
})


## Internal constructors ------------------------------------------------------

test_that("'new_bage_prior_ar' works - AR interface", {
  obj <- new_bage_prior_ar(n = 2L, min = -1, max = 1, scale = 1.0, nm = "AR")
  expect_s3_class(obj, "bage_prior_ar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 5L)
  expect_identical(obj$const, c(shape1 = 3, shape2 = 3, min = -1, max = 1, scale = 1))
  expect_identical(obj$specific, list(n = 2L,
                                      shape1 = 3,
                                      shape2 = 3,
                                      min = -1,
                                      max = 1,
                                      scale = 1,
                                      nm = "AR"))
})

test_that("'new_bage_prior_ar' works - AR1 interface", {
  obj <- new_bage_prior_ar(n = 1L, min = 0.8, max = 0.98, scale = 1.0, nm = "AR1")
  expect_s3_class(obj, "bage_prior_ar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 5L)
  expect_identical(obj$const, c(shape1 = 3, shape2 = 3, min = 0.8, max = 0.98, scale = 1))
  expect_identical(obj$specific, list(n = 1L,
                                      shape1 = 3,
                                      shape2 = 3,
                                      min = 0.8,
                                      max = 0.98,
                                      scale = 1,
                                      nm = "AR1"))
})

test_that("'new_bage_prior_compose' works - main effect", {
  priors <- list(trend = Lin(),
                 cyclical = AR(),
                 seasonal = Seas(n = 12))
  obj <- new_bage_prior_compose(priors = priors,
                                along = NULL,
                                nm = "compose_time")
  expect_s3_class(obj, "bage_prior_compose")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 1000L)
  expect_identical(obj$const, 0L)
  expect_identical(obj$specific, list(priors = priors,
                                      along = NULL,
                                      nm = "compose_time"))
})

test_that("'new_bage_prior_compose' works - interaction", {
  priors <- list(trend = ELin(),
                 cyclical = EAR(),
                 seasonal = ESeas(n = 12, along = "year"))
  obj <- new_bage_prior_compose(priors = priors,
                                along = "year",
                                nm = "compose_time")
  expect_s3_class(obj, "bage_prior_compose")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 1000L)
  expect_identical(obj$const, 0L)
  expect_identical(obj$specific, list(priors = priors,
                                      along = "year",
                                      nm = "compose_time"))
})

test_that("'new_bage_prior_ear' works - EAR interface", {
  obj <- new_bage_prior_ear(n = 2L, min = -1, max = 1, scale = 1.0, nm = "EAR", along = "age")
  expect_s3_class(obj, "bage_prior_ear")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 12L)
  expect_identical(obj$const, c(shape1 = 3, shape2 = 3, min = -1, max = 1, scale = 1))
  expect_identical(obj$specific, list(n = 2L,
                                      shape1 = 3,
                                      shape2 = 3,
                                      min = -1,
                                      max = 1,
                                      scale = 1,
                                      along = "age",
                                      nm = "EAR"))
})

test_that("'new_bage_prior_ear' works - EAR1 interface", {
  obj <- new_bage_prior_ear(n = 1L, min = 0.8, max = 0.98, scale = 1.0, nm = "EAR1", along = NULL)
  expect_s3_class(obj, "bage_prior_ear")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 12L)
  expect_identical(obj$const, c(shape1 = 3, shape2 = 3, min = 0.8, max = 0.98, scale = 1))
  expect_identical(obj$specific, list(n = 1L,
                                      shape1 = 3,
                                      shape2 = 3,
                                      min = 0.8,
                                      max = 0.98,
                                      scale = 1,
                                      along = NULL,
                                      nm = "EAR1"))
  expect_error(EAR1(along = ""), "`along` is blank.")
})

test_that("'new_bage_prior_elin' works", {
  obj <- new_bage_prior_elin(scale = 1, sd = 1, mscale = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_elin")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 9L)
  expect_identical(obj$const, c(scale = 1, sd_slope = 1, mscale = 1))
  expect_identical(obj$specific, list(scale = 1, sd_slope = 1,
                                      mscale = 1, along = NULL))
})

test_that("'new_bage_prior_erw' works", {
  obj <- new_bage_prior_erw(scale = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_erw")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 13L)
  expect_identical(obj$const, c(scale = 1))
  expect_identical(obj$specific, list(scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_eseas' works", {
  obj <- new_bage_prior_eseas(n = 2L, scale = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_eseas")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 11L)
  expect_identical(obj$const, c(scale = 1, "<unused>" = 0))
  expect_identical(length(obj$const), obj$specific$n)
  expect_identical(obj$specific, list(n = 2L,
                                      scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_known' works", {
    obj <- new_bage_prior_known(values = 1.0)
    expect_s3_class(obj, "bage_prior_known")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 0L)
    expect_identical(obj$const, 0)
    expect_identical(obj$specific, list(values = 1.0))
})

test_that("'new_bage_prior_lin' works", {
    obj <- new_bage_prior_lin(scale = 1, sd = 1)
    expect_s3_class(obj, "bage_prior_lin")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 8L)
    expect_identical(obj$const, c(scale = 1, sd_slope = 1))
    expect_identical(obj$specific, list(scale = 1, sd_slope = 1))
})

test_that("'new_bage_prior_norm' works", {
    obj <- new_bage_prior_norm(scale = 1)
    expect_s3_class(obj, "bage_prior_norm")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 1L)
    expect_identical(obj$const, c(scale = 1.0))
    expect_identical(obj$specific, list(scale = 1))
})

test_that("'new_bage_prior_normfixed' works", {
    obj <- new_bage_prior_normfixed(sd = 1)
    expect_s3_class(obj, "bage_prior_normfixed")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 2L)
    expect_identical(obj$const, c(sd = 1.0))
    expect_identical(obj$specific, list(sd = 1))
})

test_that("'new_bage_prior_rw' works", {
    obj <- new_bage_prior_rw(scale = 1)
    expect_s3_class(obj, "bage_prior_rw")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 3L)
    expect_identical(obj$const, c(scale = 1))
    expect_identical(obj$specific, list(scale = 1))
})

test_that("'new_bage_prior_rw2' works", {
    obj <- new_bage_prior_rw2(scale = 1, sd = 1)
    expect_s3_class(obj, "bage_prior_rw2")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 4L)
    expect_identical(obj$const, c(scale = 1, sd_slope = 1))
    expect_identical(obj$specific, list(scale = 1, sd_slope = 1))
})

test_that("'new_bage_prior_seas' works", {
    obj <- new_bage_prior_seas(n = 4L, scale = 1)
    expect_s3_class(obj, "bage_prior_seas")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 10L)
    expect_identical(obj$const, c(scale = 1))
    expect_identical(obj$specific, list(n = 4L, scale = 1))
})

test_that("'new_bage_prior_spline' works", {
    obj <- new_bage_prior_spline(n = 10L, scale = 1, sd_slope = 1)
    expect_s3_class(obj, "bage_prior_spline")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 6L)
    expect_identical(obj$const, c(scale = 1, sd_slope = 1))
    expect_identical(obj$specific, list(n = 10L, scale = 1, sd_slope = 1))
})

test_that("'new_bage_prior_svd' works", {
    obj <- new_bage_prior_svd(HMD,
                              nm_ssvd = "HMD",
                              n = 3L,
                              joint = NULL)
    expect_s3_class(obj, "bage_prior_svd")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 7L)
    expect_identical(obj$const, 0)
    expect_identical(obj$specific,
                     list(ssvd = HMD,
                          nm_ssvd = "HMD",
                          n = 3L,
                          joint = NULL))
    obj <- new_bage_prior_svd(HMD,
                              nm_ssvd = "HMD",
                              n = 3L,
                              joint = TRUE)
    expect_s3_class(obj, "bage_prior_svd")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 7L)
    expect_identical(obj$const, 0)
    expect_identical(obj$specific,
                     list(ssvd = HMD,
                          nm_ssvd = "HMD",
                          n = 3L,
                          joint = TRUE))
})

test_that("'new_bage_prior_esvd' works", {
  obj <- new_bage_prior_esvd(ssvd = HMD,
                             nm_ssvd = "HMD",
                             n = 3L,
                             joint = NULL)
  expect_s3_class(obj, "bage_prior_esvd")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 14L)
  expect_identical(obj$const, 0)
  expect_identical(obj$specific,
                   list(ssvd = HMD,
                        nm_ssvd = "HMD",
                        n = 3L,
                        joint = NULL))
  obj <- new_bage_prior_esvd(ssvd = HMD,
                            nm_ssvd = "HMD",
                            n = 3L,
                            joint = TRUE)
  expect_s3_class(obj, "bage_prior_esvd")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 14L)
  expect_identical(obj$const, 0)
  expect_identical(obj$specific,
                   list(ssvd = HMD,
                        nm_ssvd = "HMD",
                        n = 3L,
                        joint = TRUE))
})
