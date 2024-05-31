

## User-visible constructors --------------------------------------------------

test_that("'AR' works with valid inputs", {
  expect_identical(AR(n = 3),
                   new_bage_prior_ar(n = 3L,
                                     min = -1,
                                     max = 1,
                                     scale = 1,
                                     along = NULL,
                                     nm = "AR"))
  expect_identical(AR(n = 1, s = 0.01, along = "age"),
                   new_bage_prior_ar(n = 1L,
                                     min = -1,
                                     max = 1,
                                     scale = 0.01,
                                     along = "age",
                                     nm = "AR"))
})

test_that("'AR1' works with valid inputs", {
  expect_identical(AR1(),
                   new_bage_prior_ar(n = 1L,
                                     min = 0.8,
                                     max = 0.98,
                                     scale = 1,
                                     along = NULL,
                                     nm = "AR1"))
})

test_that("'AR1' throws current error when min >= max", {
    expect_error(AR1(min = 0.8, max = 0.8),
                 "`max` is less than or equal to `min`")
})

test_that("'Known' works with valid inputs", {
    expect_identical(Known(values = 1:3),
                     new_bage_prior_known(values = as.double(1:3)))
})

test_that("'Lin' works with valid inputs", {
  expect_identical(Lin(),
                   new_bage_prior_lin(scale = 1,
                                      sd_slope = 1,
                                      along = NULL))
  expect_identical(Lin(s = 0.3,
                       sd = 0.1,
                       along = "reg"),
                   new_bage_prior_lin(scale = 0.3,
                                      sd_slope = 0.1,
                                      along = "reg"))
})

test_that("'LinAR' works with valid inputs", {
  expect_identical(LinAR(),
                   new_bage_prior_linar(n = 2L,
                                        scale = 1,
                                        sd_slope = 1,
                                        min = -1,
                                        max = 1,
                                        along = NULL,
                                        nm = "LinAR"))
  expect_identical(LinAR(s = 0.3, sd = 0.1),
                   new_bage_prior_linar(n = 2L,
                                        scale = 0.3,
                                        sd_slope = 0.1,
                                        min = -1,
                                        max = 1,
                                        along = NULL,
                                        nm = "LinAR"))
})

test_that("'LinAR1' works with valid inputs", {
  expect_identical(LinAR1(),
                   new_bage_prior_linar(n = 1L,
                                        scale = 1,
                                        sd_slope = 1,
                                        min = 0.8,
                                        max = 0.98,
                                        along = NULL,
                                        nm = "LinAR1"))
  expect_identical(LinAR1(s = 0.3, sd = 0.1),
                   new_bage_prior_linar(n = 1L,
                                        scale = 0.3,
                                        sd_slope = 0.1,
                                        min = 0.8,
                                        max = 0.98,
                                        along = NULL,
                                        nm = "LinAR1"))
})

test_that("'N' works with valid inputs", {
    expect_identical(N(), new_bage_prior_norm(scale = 1))
})

test_that("'NFix' works with valid inputs", {
    expect_identical(NFix(), new_bage_prior_normfixed(sd = 1))
})


test_that("'RW' works with valid inputs", {
  expect_identical(RW(),
                   new_bage_prior_rw(scale = 1, along = NULL))
  expect_identical(RW(s = 0.3,
                      along = "reg"),
                   new_bage_prior_rw(scale = 0.3,
                                     along = "reg"))
})


test_that("'RWSeas' works with valid inputs", {
  expect_identical(RWSeas(n = 2, seas = 0),
                   new_bage_prior_rwseasfix(n = 2L, scale = 1, along = NULL))
  expect_identical(RWSeas(s = 0.3, n = 12, seas = 0.1, along = "reg"),
                   new_bage_prior_rwseasvary(n = 12L,
                                             scale_seas = 0.1,
                                             scale = 0.3,
                                             along = "reg"))
})


test_that("'RW2' works with valid inputs", {
  expect_identical(RW2(),
                   new_bage_prior_rw2(scale = 1,
                                      along = NULL))
  expect_identical(RW2(s = 0.3, along = "reg"),
                   new_bage_prior_rw2(scale = 0.3,
                                      along = "reg"))
})

test_that("'Sp' works with valid inputs", {
  expect_identical(Sp(),
                   new_bage_prior_spline(n = NULL,
                                         scale = 1,
                                         along = NULL))
  expect_identical(Sp(n = 6,
                      s = 3,
                      along = "time"),
                   new_bage_prior_spline(n = 6L,
                                         scale = 3,
                                         along = "time"))
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

test_that("'SVD' throws correct error when n is too high", {
  expect_error(SVD(HMD, n = 11),
               "`n` larger than number of components of `ssvd`.")
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

test_that("'SVDS' throws correct error when ssvd has no sex diemnsion", {
  ssvd <- sim_ssvd()
  ssvd$data <- ssvd$data[1,]
  expect_error(SVDS(ssvd),
               "`ssvd` does not have a sex/gender dimension.")
})


## Internal constructors ------------------------------------------------------

test_that("'new_bage_prior_ar' works - AR interface", {
  obj <- new_bage_prior_ar(n = 2L, min = -1, max = 1, scale = 1.0, along = NULL, nm = "AR")
  expect_s3_class(obj, "bage_prior_ar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 1L)
  expect_identical(obj$const,
                   c(shape1 = 2, shape2 = 2, min = -1, max = 1, scale = 1))
  expect_identical(obj$specific, list(n = 2L,
                                      shape1 = 2,
                                      shape2 = 2,
                                      min = -1,
                                      max = 1,
                                      scale = 1,
                                      along = NULL,
                                      nm = "AR"))
})

test_that("'new_bage_prior_ar' works - AR1 interface", {
  obj <- new_bage_prior_ar(n = 1L, min = 0.8, max = 0.98, scale = 1.0, along = NULL, nm = "AR1")
  expect_s3_class(obj, "bage_prior_ar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 1L)
  expect_identical(obj$const,
                   c(shape1 = 2, shape2 = 2, min = 0.8, max = 0.98, scale = 1))
  expect_identical(obj$specific, list(n = 1L,
                                      shape1 = 2,
                                      shape2 = 2,
                                      min = 0.8,
                                      max = 0.98,
                                      scale = 1,
                                      along = NULL,
                                      nm = "AR1"))
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
  obj <- new_bage_prior_lin(scale = 1, sd = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_lin")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 2L)
  expect_identical(obj$const, c(scale = 1, sd_slope = 1))
  expect_identical(obj$specific, list(scale = 1, sd_slope = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_linar' works - AR interface", {
  obj <- new_bage_prior_linar(n = 2L, scale = 1, sd = 1, min = -1, max = 1,
                              along = NULL, nm = "LinAR")
  expect_s3_class(obj, "bage_prior_linar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 3L)
  expect_identical(obj$const, c(scale = 1,
                                sd_slope = 1,
                                shape1 = 2,
                                shape2 = 2,
                                min = -1,
                                max = 1))
  expect_identical(obj$specific,
                   list(n = 2L,
                        scale = 1,
                        sd_slope = 1,
                        shape1 = 2,
                        shape2 = 2,
                        min = -1,
                        max = 1,
                        along = NULL,
                        nm = "LinAR"))
})

test_that("'new_bage_prior_linar' works - AR1 interface", {
  obj <- new_bage_prior_linar(n = 1L, scale = 1, sd = 1, min = 0.8, max = 0.98,
                              along = NULL, nm = "LinAR1")
  expect_s3_class(obj, "bage_prior_linar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 3L)
  expect_identical(obj$const, c(scale = 1,
                                sd_slope = 1,
                                shape1 = 2,
                                shape2 = 2,
                                min = 0.8,
                                max = 0.98))
  expect_identical(obj$specific,
                   list(n = 1L,
                        scale = 1,
                        sd_slope = 1,
                        shape1 = 2,
                        shape2 = 2,
                        min = 0.8,
                        max = 0.98,
                        along = NULL,
                        nm = "LinAR1"))
})

test_that("'new_bage_prior_norm' works", {
    obj <- new_bage_prior_norm(scale = 1)
    expect_s3_class(obj, "bage_prior_norm")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 4L)
    expect_identical(obj$const, c(scale = 1.0))
    expect_identical(obj$specific, list(scale = 1))
})

test_that("'new_bage_prior_normfixed' works", {
    obj <- new_bage_prior_normfixed(sd = 1)
    expect_s3_class(obj, "bage_prior_normfixed")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 5L)
    expect_identical(obj$const, c(sd = 1.0))
    expect_identical(obj$specific, list(sd = 1))
})

test_that("'new_bage_prior_rw' works", {
  obj <- new_bage_prior_rw(scale = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_rw")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 6L)
  expect_identical(obj$const, c(scale = 1))
  expect_identical(obj$specific, list(scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_rwseasfix' works", {
  obj <- new_bage_prior_rwseasfix(n = 4, scale = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_rwseasfix")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 10L)
  expect_identical(obj$const, c(n = 4, scale = 1))
  expect_identical(obj$specific, list(n = 4,
                                      scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_rwseasvary' works", {
  obj <- new_bage_prior_rwseasvary(n = 4, scale = 1, scale_seas = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_rwseasvary")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 11L)
  expect_identical(obj$const, c(n = 4, scale_seas = 1, scale = 1))
  expect_identical(obj$specific, list(n = 4,
                                      scale_seas = 1,
                                      scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_rw2' works", {
  obj <- new_bage_prior_rw2(scale = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_rw2")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 7L)
  expect_identical(obj$const, c(scale = 1))
  expect_identical(obj$specific, list(scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_spline' works", {
    obj <- new_bage_prior_spline(n = 10L, scale = 1, along = NULL)
    expect_s3_class(obj, "bage_prior_spline")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 8L)
    expect_identical(obj$const, c(scale = 1))
    expect_identical(obj$specific,
                     list(n = 10L, scale = 1, along = NULL))
})

test_that("'new_bage_prior_svd' works", {
    obj <- new_bage_prior_svd(HMD,
                              nm_ssvd = "HMD",
                              n = 3L,
                              joint = NULL)
    expect_s3_class(obj, "bage_prior_svd")
    expect_s3_class(obj, "bage_prior")
    expect_identical(obj$i_prior, 9L)
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
    expect_identical(obj$i_prior, 9L)
    expect_identical(obj$const, 0)
    expect_identical(obj$specific,
                     list(ssvd = HMD,
                          nm_ssvd = "HMD",
                          n = 3L,
                          joint = TRUE))
})
