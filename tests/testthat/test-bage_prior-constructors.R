
## User-visible constructors --------------------------------------------------

test_that("'AR' works with valid inputs", {
  expect_identical(AR(n_coef = 3),
                   new_bage_prior_ar(n_coef = 3L,
                                     min = -1,
                                     max = 1,
                                     scale = 1,
                                     along = NULL,
                                     nm = "AR"))
  expect_identical(AR(n_coef = 1, s = 0.01, along = "age"),
                   new_bage_prior_ar(n_coef = 1L,
                                     min = -1,
                                     max = 1,
                                     scale = 0.01,
                                     along = "age",
                                     nm = "AR"))
})

test_that("'AR1' works with valid inputs", {
  expect_identical(AR1(),
                   new_bage_prior_ar(n_coef = 1L,
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

test_that("'Lin_AR' works with valid inputs", {
  expect_identical(Lin_AR(),
                   new_bage_prior_linar(n_coef = 2L,
                                        scale = 1,
                                        sd_slope = 1,
                                        min = -1,
                                        max = 1,
                                        along = NULL,
                                        nm = "Lin_AR"))
  expect_identical(Lin_AR(s = 0.3, sd = 0.1),
                   new_bage_prior_linar(n_coef = 2L,
                                        scale = 0.3,
                                        sd_slope = 0.1,
                                        min = -1,
                                        max = 1,
                                        along = NULL,
                                        nm = "Lin_AR"))
})

test_that("'Lin_AR1' works with valid inputs", {
  expect_identical(Lin_AR1(),
                   new_bage_prior_linar(n_coef = 1L,
                                        scale = 1,
                                        sd_slope = 1,
                                        min = 0.8,
                                        max = 0.98,
                                        along = NULL,
                                        nm = "Lin_AR1"))
  expect_identical(Lin_AR1(s = 0.3, sd = 0.1),
                   new_bage_prior_linar(n_coef = 1L,
                                        scale = 0.3,
                                        sd_slope = 0.1,
                                        min = 0.8,
                                        max = 0.98,
                                        along = NULL,
                                        nm = "Lin_AR1"))
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


test_that("'RW_Seas' works with valid inputs", {
  expect_identical(RW_Seas(n_seas = 2, s_seas = 0),
                   new_bage_prior_rwseasfix(n_seas = 2L, scale = 1, along = NULL))
  expect_identical(RW_Seas(s = 0.3, n_seas = 12, s_seas = 0.1, along = "reg"),
                   new_bage_prior_rwseasvary(n_seas = 12L,
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

test_that("'RW2_Seas' works with valid inputs", {
  expect_identical(RW2_Seas(n_seas = 2, s_seas = 0),
                   new_bage_prior_rw2seasfix(n_seas = 2L, scale = 1, along = NULL))
  expect_identical(RW2_Seas(s = 0.3, n_seas = 12, s_seas = 0.1, along = "reg"),
                   new_bage_prior_rw2seasvary(n_seas = 12L,
                                              scale_seas = 0.1,
                                              scale = 0.3,
                                              along = "reg"))
})

test_that("'Sp' works with valid inputs", {
  expect_identical(Sp(),
                   new_bage_prior_spline(n_comp = NULL,
                                         scale = 1,
                                         along = NULL))
  expect_identical(Sp(n_comp = 6,
                      s = 3,
                      along = "time"),
                   new_bage_prior_spline(n_comp = 6L,
                                         scale = 3,
                                         along = "time"))
})

test_that("'SVD' works with valid inputs", {
    expect_identical(SVD(HMD),
                     new_bage_prior_svd(HMD,
                                        nm_ssvd = "HMD",
                                        indep = TRUE,
                                        n_comp = 3L))
    expect_identical(SVD(HMD, n_comp = 2),
                     new_bage_prior_svd(HMD,
                                        nm_ssvd = "HMD",
                                        indep = TRUE,
                                        n_comp = 2L))
    expect_identical(SVD(HMD, n_comp = 4, indep = FALSE),
                     new_bage_prior_svd(HMD,
                                        nm_ssvd = "HMD",
                                        indep = FALSE,
                                        n_comp = 4L))
})

test_that("'SVD' throws correct error when n_comp is too high", {
  expect_error(SVD(HMD, n_comp = 11),
               "`n_comp` larger than number of components of `ssvd`.")
})

test_that("'SVD_AR' works with valid inputs", {
  expect_identical(SVD_AR(HMD, n_coef = 3),
                   new_bage_prior_svd_ar(HMD,
                                         nm_ssvd = "HMD",
                                         n_comp = 3L,
                                         indep = TRUE,
                                         n_coef = 3L,
                                         scale = 1,
                                         min = -1,
                                         max = 1,
                                         nm = "SVD_AR"))
  expect_identical(SVD_AR(LFP, n_comp = 2, indep = FALSE, n_coef = 1, s = 0.01),
                   new_bage_prior_svd_ar(LFP,
                                         nm_ssvd = "LFP",
                                         n_comp = 2L,
                                         indep = FALSE,
                                         n_coef = 1L,
                                         min = -1,
                                         max = 1,
                                         scale = 0.01,
                                         nm = "SVD_AR"))
})

test_that("'SVD_AR1' works with valid inputs", {
  expect_identical(SVD_AR1(HMD),
                   new_bage_prior_svd_ar(HMD,
                                         nm_ssvd = "HMD",
                                         n_comp = 3L,
                                         indep = TRUE,
                                         n_coef = 1L,
                                         scale = 1,
                                         min = 0.8,
                                         max = 0.98,
                                         nm = "SVD_AR1"))
  expect_identical(SVD_AR1(LFP, n_comp = 2, max = 0.9, s = 0.01),
                   new_bage_prior_svd_ar(LFP,
                                         nm_ssvd = "LFP",
                                         n_comp = 2L,
                                         indep = TRUE,
                                         n_coef = 1L,
                                         min = 0.8,
                                         max = 0.9,
                                         scale = 0.01,
                                         nm = "SVD_AR1"))
})

test_that("'SVD_RW' works with valid inputs", {
  expect_identical(SVD_RW(HMD),
                   new_bage_prior_svd_rw(HMD,
                                         nm_ssvd = "HMD",
                                         n_comp = 3L,
                                         indep = TRUE,
                                         scale = 1))
  expect_identical(SVD_RW(LFP, n_comp = 2, s = 0.01, indep = F),
                   new_bage_prior_svd_rw(LFP,
                                         nm_ssvd = "LFP",
                                         n_comp = 2L,
                                         indep = FALSE,
                                         scale = 0.01))
})

test_that("'SVD_RW2' works with valid inputs", {
  expect_identical(SVD_RW2(HMD),
                   new_bage_prior_svd_rw2(HMD,
                                          nm_ssvd = "HMD",
                                          n_comp = 3L,
                                          indep = TRUE,
                                          scale = 1))
  expect_identical(SVD_RW2(LFP, n_comp = 2, s = 0.01),
                   new_bage_prior_svd_rw2(LFP,
                                          nm_ssvd = "LFP",
                                          n_comp = 2L,
                                          indep = TRUE,
                                          scale = 0.01))
})


## Internal constructors ------------------------------------------------------

test_that("'new_bage_prior_ar' works - AR interface", {
  obj <- new_bage_prior_ar(n_coef = 2L,
                           min = -1,
                           max = 1,
                           scale = 1.0,
                           along = NULL,
                           nm = "AR")
  expect_s3_class(obj, "bage_prior_ar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 1L)
  expect_identical(obj$const,
                   c(shape1 = 2, shape2 = 2, min = -1, max = 1, scale = 1))
  expect_identical(obj$specific, list(n_coef = 2L,
                                      shape1 = 2,
                                      shape2 = 2,
                                      min = -1,
                                      max = 1,
                                      scale = 1,
                                      along = NULL,
                                      nm = "AR"))
})

test_that("'new_bage_prior_ar' works - AR1 interface", {
  obj <- new_bage_prior_ar(n_coef = 1L,
                           min = 0.8,
                           max = 0.98,
                           scale = 1.0,
                           along = NULL,
                           nm = "AR1")
  expect_s3_class(obj, "bage_prior_ar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 1L)
  expect_identical(obj$const,
                   c(shape1 = 2,
                     shape2 = 2,
                     min = 0.8,
                     max = 0.98,
                     scale = 1))
  expect_identical(obj$specific,
                   list(n_coef = 1L,
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
  obj <- new_bage_prior_linar(n_coef = 2L,
                              scale = 1,
                              sd = 1,
                              min = -1,
                              max = 1,
                              along = NULL,
                              nm = "Lin_AR")
  expect_s3_class(obj, "bage_prior_linar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 3L)
  expect_identical(obj$const,
                   c(scale = 1,
                     sd_slope = 1,
                     shape1 = 2,
                     shape2 = 2,
                     min = -1,
                     max = 1))
  expect_identical(obj$specific,
                   list(n_coef = 2L,
                        scale = 1,
                        sd_slope = 1,
                        shape1 = 2,
                        shape2 = 2,
                        min = -1,
                        max = 1,
                        along = NULL,
                        nm = "Lin_AR"))
})

test_that("'new_bage_prior_linar' works - AR1 interface", {
  obj <- new_bage_prior_linar(n_coef = 1L,
                              scale = 1,
                              sd = 1,
                              min = 0.8,
                              max = 0.98,
                              along = NULL,
                              nm = "Lin_AR1")
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
                   list(n_coef = 1L,
                        scale = 1,
                        sd_slope = 1,
                        shape1 = 2,
                        shape2 = 2,
                        min = 0.8,
                        max = 0.98,
                        along = NULL,
                        nm = "Lin_AR1"))
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
  obj <- new_bage_prior_rwseasfix(n_seas = 4,
                                  scale = 1,
                                  along = NULL)
  expect_s3_class(obj, "bage_prior_rwseasfix")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 10L)
  expect_identical(obj$const, c(n_seas = 4, scale = 1))
  expect_identical(obj$specific, list(n_seas = 4,
                                      scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_rwseasvary' works", {
  obj <- new_bage_prior_rwseasvary(n_seas = 4,
                                   scale = 1,
                                   scale_seas = 1,
                                   along = NULL)
  expect_s3_class(obj, "bage_prior_rwseasvary")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 11L)
  expect_identical(obj$const, c(n_seas = 4,
                                scale_seas = 1,
                                scale = 1))
  expect_identical(obj$specific,
                   list(n_seas = 4,
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
  expect_identical(obj$specific,
                   list(scale = 1,
                        along = NULL))
})

test_that("'new_bage_prior_rw2seasfix' works", {
  obj <- new_bage_prior_rw2seasfix(n_seas = 4, scale = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_rw2seasfix")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 12L)
  expect_identical(obj$const, c(n_seas = 4, scale = 1))
  expect_identical(obj$specific, list(n_seas = 4,
                                      scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_rw2seasvary' works", {
  obj <- new_bage_prior_rw2seasvary(n_seas = 4, scale = 1, scale_seas = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_rw2seasvary")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 13L)
  expect_identical(obj$const, c(n_seas = 4, scale_seas = 1, scale = 1))
  expect_identical(obj$specific, list(n_seas = 4,
                                      scale_seas = 1,
                                      scale = 1,
                                      along = NULL))
})

test_that("'new_bage_prior_spline' works", {
  obj <- new_bage_prior_spline(n_comp = 10L, scale = 1, along = NULL)
  expect_s3_class(obj, "bage_prior_spline")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 8L)
  expect_identical(obj$const, c(scale = 1))
  expect_identical(obj$specific,
                   list(n_comp = 10L, scale = 1, along = NULL))
})

test_that("'new_bage_prior_svd' works", {
  obj <- new_bage_prior_svd(HMD,
                            nm_ssvd = "HMD",
                            n_comp = 3L,
                            indep = TRUE)
  expect_s3_class(obj, "bage_prior_svd")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 9L)
  expect_identical(obj$const, 0)
  expect_identical(obj$specific,
                   list(ssvd = HMD,
                        nm_ssvd = "HMD",
                        n_comp = 3L,
                        indep = TRUE))
  obj <- new_bage_prior_svd(HMD,
                            nm_ssvd = "HMD",
                            n_comp = 3L,
                            indep = FALSE)
  expect_s3_class(obj, "bage_prior_svd")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 9L)
  expect_identical(obj$const, 0)
  expect_identical(obj$specific,
                   list(ssvd = HMD,
                        nm_ssvd = "HMD",
                        n_comp = 3L,
                        indep = FALSE))
})

test_that("'new_bage_prior_svd_ar' works", {
  obj <- new_bage_prior_svd_ar(HMD,
                               nm_ssvd = "HMD",
                               n_comp = 3L,
                               indep = TRUE,
                               n_coef = 2L,
                               scale = 1,
                               min = -1,
                               max = 1,
                               nm = "SVD_AR")
  expect_s3_class(obj, "bage_prior_svd_ar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 14L)
  expect_identical(obj$const,
                   c(shape1 = 2,
                     shape2 = 2,
                     min = -1,
                     max = 1,
                     scale = 1))
  expect_identical(obj$specific,
                   list(ssvd = HMD,
                        nm_ssvd = "HMD",
                        n_comp = 3L,
                        indep = TRUE,
                        n_coef = 2L,
                        shape1 = 2,
                        shape2 = 2,
                        min = -1,
                        max = 1,
                        scale = 1,
                        nm = "SVD_AR",
                        along = NULL))
  obj <- new_bage_prior_svd_ar(HMD,
                               nm_ssvd = "HMD",
                               n_comp = 5L,
                               indep = FALSE,
                               n_coef = 1L,
                               scale = 0.5,
                               min = 0.8,
                               max = 0.98,
                               nm = "SVD_AR1")
  expect_s3_class(obj, "bage_prior_svd_ar")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 14L)
  expect_identical(obj$const,
                   c(shape1 = 2,
                     shape2 = 2,
                     min = 0.8,
                     max = 0.98,
                     scale = 0.5))
  expect_identical(obj$specific,
                   list(ssvd = HMD,
                        nm_ssvd = "HMD",
                        n_comp = 5L,
                        indep = FALSE,
                        n_coef = 1L,
                        shape1 = 2,
                        shape2 = 2,
                        min = 0.8,
                        max = 0.98,
                        scale = 0.5,
                        nm = "SVD_AR1",
                        along = NULL))
})

test_that("'new_bage_prior_svd_rw' works", {
  obj <- new_bage_prior_svd_rw(HMD,
                               nm_ssvd = "HMD",
                               n_comp = 3L,
                               indep = TRUE,
                               scale = 1)
  expect_s3_class(obj, "bage_prior_svd_rw")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 15L)
  expect_identical(obj$const,
                   c(scale = 1))
  expect_identical(obj$specific,
                   list(ssvd = HMD,
                        nm_ssvd = "HMD",
                        n_comp = 3L,
                        indep = TRUE,
                        scale = 1,
                        along = NULL))
})

test_that("'new_bage_prior_svd_rw2' works", {
  obj <- new_bage_prior_svd_rw2(HMD,
                                nm_ssvd = "HMD",
                                n_comp = 3L,
                                indep = TRUE,
                                scale = 1)
  expect_s3_class(obj, "bage_prior_svd_rw2")
  expect_s3_class(obj, "bage_prior")
  expect_identical(obj$i_prior, 16L)
  expect_identical(obj$const,
                   c(scale = 1))
  expect_identical(obj$specific,
                   list(ssvd = HMD,
                        nm_ssvd = "HMD",
                        n_comp = 3L,
                        indep = TRUE,
                        scale = 1,
                        along = NULL))
})


