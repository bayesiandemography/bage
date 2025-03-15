
## comp_hyperrand ---------------------------------------------------------------

test_that("'comp_hyperrand' works with 'bage_prior_ar'", {
  dimnames_term <- list(time = 2001:2010)
  ans_obtained <- comp_hyperrand(prior = AR(n_coef = 2),
                                 dimnames_term = dimnames_term)
  ans_expected <- character()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'comp_hyperrand' works with 'bage_prior_lin'", {
  dimnames_term <- list(x = letters[1:13], y = c("a", "b"))
  ans_obtained <- comp_hyperrand(prior = Lin(along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("hyper", "trend", "error"), times = c(2, 26, 26))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_linar'", {
  dimnames_term <- list(x = letters[1:13],
                        y = c("a", "b"))
  ans_obtained <- comp_hyperrand(prior = Lin_AR(along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("hyper", "trend", "error"), times = c(2, 26, 26))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_linar' - n_by = 1", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- comp_hyperrand(prior = Lin_AR(along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("hyper", "trend", "error"), times = c(1, 13, 13))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rwrandomseasfix' - main effect", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- comp_hyperrand(prior = RW_Seas(n_seas = 3, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 13)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rwrandomseasfix' - interaction", {
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2010)
  ans_obtained <- comp_hyperrand(prior = RW_Seas(n_seas = 3),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 20)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rwrandomseasvary' - interaction", {
  dimnames_term <- list(time = 2001:2013,
                        sex = c("f", "m"))
  ans_obtained <- comp_hyperrand(prior = RW_Seas(n_seas = 3, s_seas = 1),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 26)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rwzeroseasfix' - main effect", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- comp_hyperrand(prior = RW_Seas(n_seas = 3, sd = 0, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 13)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rwzeroseasfix' - interaction", {
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2010)
  ans_obtained <- comp_hyperrand(prior = RW_Seas(n_seas = 3, sd = 0),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 20)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rwzeroseasvary' - interaction", {
  dimnames_term <- list(time = 2001:2013,
                        sex = c("f", "m"))
  ans_obtained <- comp_hyperrand(prior = RW_Seas(n_seas = 3, s_seas = 1, sd = 0),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 26)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rw2randomseasfix'", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- comp_hyperrand(prior = RW2_Seas(n_seas = 3, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 13)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rw2randomseasvary'", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- comp_hyperrand(prior = RW2_Seas(n_seas = 3, s_seas = 1, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 13)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rw2zeroseasfix'", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- comp_hyperrand(prior = RW2_Seas(n_seas = 3, sd = 0, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 13)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'comp_hyperrand' works with 'bage_prior_rw2zeroseasvary'", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- comp_hyperrand(prior = RW2_Seas(n_seas = 3, sd = 0, s_seas = 1, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(c("trend", "season"), each = 13)
  expect_identical(ans_obtained, ans_expected)                   
})


## const ----------------------------------------------------------------------

test_that("'const' works with bage_prior_ar", {
  prior <- AR(n_coef = 3)
  ans_obtained <- const(prior)
  ans_expected <- prior$const
  expect_identical(ans_obtained, ans_expected)
})


## draw_vals_effect --------------------------------------------------------------

test_that("'draw_vals_effect' works with bage_prior_ar - n_by = 1", {
  prior <- AR(n_coef = 3, along = "x")
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(x = letters)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(letters, NULL))
})

test_that("'draw_vals_effect' works with bage_prior_ar - n_by = 2, con is 'none'", {
  prior <- AR(n_coef = 3, along = "x")
  dimnames_term <- list(x = letters[1:13], reg = c("a", "b"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(26L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_ar - n_by = 2, con is 'by'", {
  prior <- AR(n_coef = 3, along = "x", con = "by")
  dimnames_term <- list(x = letters[1:13], reg = c("a", "b"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  levels_effect <- letters
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(26L, 10L))
  a <- array(ans, dim = c(13, 2, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 130))
})

test_that("'draw_vals_effect' works with bage_prior_known", {
  prior <- Known(c(-0.1, 0, 0.1))
  n_sim <- 10
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(x = c("a", "b", "c"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = NULL,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(c("a", "b", "c"), NULL))
})

test_that("'draw_vals_effect' works with bage_prior_lin - n_by = 2, con is 'none'", {
  prior <- Lin(along = "x")
  dimnames_term <- list(x = 1:13, y = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(26L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_lin - n_by = 2, con is 'by'", {
  prior <- Lin(along = "x", con = "by")
  dimnames_term <- list(x = 1:13, y = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(26L, 10L))
  a <- array(ans, dim = c(13, 2, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 130))
})

test_that("'draw_vals_effect' works with bage_prior_lin - n_by = 4", {
  prior <- Lin(s = 0.01)
  dimnames_term <- list(time = 1:10, y = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_linar", {
  prior <- Lin_AR()
  dimnames_term <- list(time = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(10L, 10L))
})


test_that("'draw_vals_effect' works with bage_prior_linar, con is 'by'", {
  prior <- Lin_AR(con = "by")
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_linex - n_by = 2, con is 'none'", {
  prior <- Lin(along = "x", s = 0)
  dimnames_term <- list(x = 1:13, y = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- NULL
  vals_hyperrand <- NULL
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(26L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_linex - n_by = 2, con is 'by'", {
  prior <- Lin(along = "x", con = "by", s = 0)
  dimnames_term <- list(x = 1:13, y = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  n_sim <- 10
  vals_hyper <- NULL
  vals_hyperrand <- NULL
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(26L, 10L))
  a <- array(ans, dim = c(13, 2, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 130))
})

test_that("'draw_vals_effect' works with bage_prior_norm", {
  prior <- N()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(region = 1:1000)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyperrand = vals_hyperrand,
                          vals_hyper = vals_hyper,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(as.character(1:1000), NULL))
  expect_equal(unname(apply(ans, 2, sd)), vals_hyper$sd, tolerance = 0.05)
})

test_that("'draw_vals_effect' works with bage_prior_normfixed", {
  prior <- NFix(sd = 0.3)
  n_sim <- 10
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(region = 1:1000)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = NULL,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dimnames(ans), list(as.character(1:1000), NULL))
  expect_equal(unname(apply(ans, 2, sd)), rep(0.3, 10), tolerance = 0.05)
})

test_that("'draw_vals_effect' works with bage_prior_rwrandom - n_by = 1", {
  prior <- RW()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(time = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(10L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rwrandom - n_by = 4, con is 'none'", {
  prior <- RW(s = 0.01)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rwrandom - n_by = 4, con is 'by'", {
  prior <- RW(s = 0.01, con = "by")
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rwrandomseasfix - con is 'none'", {
  prior <- RW_Seas(n_seas = 2, s = 0.01)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rwrandomseasfix - con is 'by'", {
  prior <- RW_Seas(n_seas = 2, s = 0.01, con = "by")
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rwrandomseasvary, con is 'none'", {
  prior <- RW_Seas(n_seas = 2, s = 0.01, s_seas = 0.5)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rwrandomseasvary, con is 'by'", {
  prior <- RW_Seas(n_seas = 2, s = 0.01, s_seas = 0.5, con = "by")
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rwzero - n_by = 1", {
  prior <- RW(sd = 0)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(time = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(10L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rwzero - n_by = 4, con is 'none'", {
  prior <- RW(s = 0.01, sd = 0)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rwzero - n_by = 4, con is 'by'", {
  prior <- RW(s = 0.01, sd = 0, con = "by")
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  vals_svd <- NULL
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rwzeroseasfix - con is 'none'", {
  prior <- RW_Seas(n_seas = 2, sd = 0, s = 0.01)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rwzeroseasfix - con is 'by'", {
  prior <- RW_Seas(n_seas = 2, s = 0.01, sd = 0, con = "by")
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rwzeroseasvary, con is 'none'", {
  prior <- RW_Seas(n_seas = 2, s = 0.01, sd = 0, s_seas = 0.5)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rwzeroseasvary, con is 'by'", {
  prior <- RW_Seas(n_seas = 2, s = 0.01, s_seas = 0.5, sd = 0, con = "by")
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rw2infant - n_by = 4, con is 'none'", {
  prior <- RW2_Infant()
  n_sim <- 10
  dimnames_term <- list(age = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  set.seed(0)
  ans_obtained <- draw_vals_effect(prior = prior,
                                   vals_hyper = vals_hyper,
                                   vals_hyperrand = vals_hyperrand,
                                   vals_spline = vals_spline,
                                   vals_svd = vals_svd,
                                   dimnames_term = dimnames_term,
                                   var_time = var_time,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender,
                                   n_sim = n_sim)
  set.seed(0)
  ans_expected <- draw_vals_effect(prior = RW2(along = "age", sd = 0),
                                   vals_hyper = vals_hyper,
                                   vals_hyperrand = vals_hyperrand,
                                   vals_spline = vals_spline,
                                   vals_svd = vals_svd,
                                   dimnames_term = dimnames_term,
                                   var_time = var_time,
                                   var_age = var_age,
                                   var_sexgender = var_sexgender,
                                   n_sim = n_sim)
  ans_expected[c(1, 11, 21, 31)] <- rnorm(4)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_effect' works with bage_prior_rw2infant - n_by = 4, con = 'by'", {
  prior <- RW2_Infant(con = "by")
  n_sim <- 10
  dimnames_term <- list(age = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rw2random - n_by = 4, con is 'none'", {
  prior <- RW2()
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rw2random - n_by = 4, con = 'by'", {
  prior <- RW2(con = 'by')
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rw2randomseasfix - con is 'none'", {
  prior <- RW2_Seas(n_seas = 2, s = 0.01)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rw2randomseasfix - con is 'by'", {
  prior <- RW2_Seas(n_seas = 2, s = 0.01, con = 'by')
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rw2randomseasvary, con is 'none'", {
  prior <- RW2_Seas(n_seas = 2, s = 0.01, s_seas = 0.5)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rw2randomseasvary, con is 'by'", {
  prior <- RW2_Seas(n_seas = 2, s = 0.01, s_seas = 0.5, con = 'by')
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rw2zero - n_by = 4, con is 'none'", {
  prior <- RW2(sd = 0)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rw2zero - n_by = 4, con = 'by'", {
  prior <- RW2(sd = 0, con = 'by')
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rw2zeroseasfix - con is 'none'", {
  prior <- RW2_Seas(n_seas = 2, s = 0.01, sd = 0)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rw2zeroseasfix - con is 'by'", {
  prior <- RW2_Seas(n_seas = 2, s = 0.01, sd = 0, con = 'by')
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_rw2zeroseasvary, con is 'none'", {
  prior <- RW2_Seas(n_seas = 2, s = 0.01, sd = 0, s_seas = 0.5)
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_rw2zeroseasvary, con is 'by'", {
  prior <- RW2_Seas(n_seas = 2, s = 0.01, s_seas = 0.5, sd = 0, con = 'by')
  n_sim <- 10
  dimnames_term <- list(time = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- draw_vals_hyperrand(prior = prior,
                                        vals_hyper = vals_hyper,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        n_sim = n_sim)
  vals_spline <- NULL
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_spline - n_by = 1", {
  prior <- Sp(n_comp = 5)
  n_sim <- 10
  dimnames_term <- list(age = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- NULL
  levels_spline <- paste0("comp", 1:5)
  vals_spline <- draw_vals_spline(prior = prior,
                                  vals_hyper = vals_hyper,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  levels_spline = levels_spline)
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(10L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_spline - n_by = 4, con is 'none'", {
  prior <- Sp(n_comp = 5)
  n_sim <- 10
  dimnames_term <- list(age = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- NULL
  levels_spline <- paste(paste0("comp", 1:5), rep(1:4, each = 5), sep = ".")
  vals_spline <- draw_vals_spline(prior = prior,
                                  vals_hyper = vals_hyper,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  levels_spline = levels_spline)
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_spline - n_by = 4, con = 'by'", {
  prior <- Sp(n_comp = 5, con = 'by')
  n_sim <- 10
  dimnames_term <- list(age = 1:10, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- NULL
  levels_spline <- paste(paste0("comp", 1:5), rep(1:3, each = 5), sep = ".")
  vals_spline <- draw_vals_spline(prior = prior,
                                  vals_hyper = vals_hyper,
                                  dimnames_term = dimnames_term,
                                  var_time = var_time,
                                  var_age = var_age,
                                  levels_spline = levels_spline)
  vals_svd <- NULL
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(40L, 10L))
  a <- array(ans, dim = c(10, 4, 10))
  expect_equal(as.numeric(apply(a, c(1, 3), mean)), rep(0, 100))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age main effect", {
  prior <- SVD(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  vals_spline <- NULL
  dimnames_term <- list(age = c(0:59, "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_svd = paste0("comp", 1:3),
                            n_sim = n_sim)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(61L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd, with region", {
  prior <- SVD(HMD)
  n_sim <- 10
  dimnames_term <- list(region = c("A", "B"),
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_svd <- paste(paste0("comp", 1:3), rep(c("A", "B"), each = 3), sep = ".")
  vals_spline <- NULL
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_svd = levels_svd,
                            n_sim = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(81L * 2L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age:sex interaction - indep", {
  prior <- SVD(HMD)
  n_sim <- 10
  dimnames_term <- list(sex = c("F", "M"),
                        age = c(0:79, "80+"),
                        region = c("A", "B"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_svd <- paste(rep(c("F", "M"), each = 3),
                             paste0("comp", 1:3),
                             rep(c("A", "B"), each = 6), sep = ".")
  vals_spline <- NULL
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            levels_svd = levels_svd,
                            n_sim = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(324L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd - age:sex interaction - joint", {
  prior <- SVD(HMD, indep = FALSE)
  n_sim <- 10
  dimnames_term <- list(sex = c("F", "M"),
                        age = c(0:79, "80+"),
                        region = c("A", "B"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_svd <- paste(paste0("comp", 1:3),
                             rep(c("A", "B"), each = 3), sep = ".")
  vals_spline <- NULL
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            levels_svd = levels_svd,
                            n_sim = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(324L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd_ar - age x time", {
  prior <- SVD_AR1(HMD)
  n_sim <- 10
  dimnames_term <- list(time = 2000:2004,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_svd <- paste(paste0("comp", 1:3), rep(2000:2004, each = 3), sep = ".")
  vals_spline <- NULL
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_svd = levels_svd,
                            n_sim = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(5L * 81L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd_rwrandom - age x time", {
  prior <- SVD_RW(HMD)
  n_sim <- 10
  dimnames_term <- list(time = 2000:2004,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  matrix_along_by_free <- matrix(0:29, nr = 5)
  levels_svd <- paste(paste0("comp", 1:3), rep(2000:2004, each = 3), sep = ".")
  vals_spline <- NULL
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_svd = levels_svd,
                            n_sim = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(5L * 81L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd_rwzero - age x time", {
  prior <- SVD_RW(HMD, sd = 0)
  n_sim <- 10
  dimnames_term <- list(time = 2000:2004,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  matrix_along_by_free <- matrix(0:29, nr = 5)
  levels_svd <- paste(paste0("comp", 1:3), rep(2000:2004, each = 3), sep = ".")
  vals_spline <- NULL
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_svd = levels_svd,
                            n_sim = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(5L * 81L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd_rw2random - age and sex", {
  prior <- SVD_RW2(HMD, con = 'by')
  n_sim <- 10
  dimnames_term <- list(age = c(0:79, "80+"),
                        sex = c("F", "M"),
                        time = 2000:2004)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_svd <- paste(rep(c("F", "M"), each = 3),
                             paste0("comp", 1:3),
                             rep(2000:2004, each = 6),
                             sep = ".")
  vals_spline <- NULL
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_svd = levels_svd,
                            n_sim = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(81L * 2L * 5L, 10L))
})

test_that("'draw_vals_effect' works with bage_prior_svd_rw2zero - age and sex", {
  prior <- SVD_RW2(HMD, sd = 0, con = 'by')
  n_sim <- 10
  dimnames_term <- list(age = c(0:79, "80+"),
                        sex = c("F", "M"),
                        time = 2000:2004)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  vals_hyperrand <- list()
  levels_svd <- paste(rep(c("F", "M"), each = 3),
                             paste0("comp", 1:3),
                             rep(2000:2004, each = 6),
                             sep = ".")
  vals_spline <- NULL
  vals_svd <- draw_vals_svd(prior = prior,
                            vals_hyper = vals_hyper,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender,
                            levels_svd = levels_svd,
                            n_sim = 10)
  ans <- draw_vals_effect(prior = prior,
                          vals_hyper = vals_hyper,
                          vals_hyperrand = vals_hyperrand,
                          vals_spline = vals_spline,
                          vals_svd = vals_svd,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          var_sexgender = var_sexgender,
                          n_sim = n_sim)
  expect_identical(dim(ans), c(81L * 2L * 5L, 10L))
})



## draw_vals_hyper ------------------------------------------------------------

test_that("'draw_vals_hyper' works with bage_prior_ar", {
  prior <- AR(n_coef = 3)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("coef", "sd"))
  expect_identical(dim(ans$coef), c(3L, 10L))
})

test_that("'draw_vals_hyper' works with bage_prior_known", {
  prior <- Known(c(0.1, 0.3))
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_lin", {
  prior <- Lin()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_linar", {
  prior <- Lin_AR()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("coef", "sd"))
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_linex", {
  prior <- Lin(s = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_norm", {
  prior <- N()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_normfixed", {
  prior <- NFix()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_rwrandom", {
  prior <- RW()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rwrandomseasfix", {
  prior <- RW_Seas(n_seas = 2)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rwrandomseasvary", {
  prior <- RW_Seas(n_seas = 2, s_seas = 0.1)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("sd_seas", "sd"))
  expect_identical(length(ans$sd_seas), 10L)
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rwzero", {
  prior <- RW(sd = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rwzeroseasfix", {
  prior <- RW_Seas(n_seas = 2, sd = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rwzeroseasvary", {
  prior <- RW_Seas(n_seas = 2, s_seas = 0.1, sd = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("sd_seas", "sd"))
  expect_identical(length(ans$sd_seas), 10L)
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2infant", {
  prior <- RW2_Infant()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2random", {
  prior <- RW2()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2randomseasfix", {
  prior <- RW2_Seas(n_seas = 2)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2randomseasvary", {
  prior <- RW2_Seas(n_seas = 2, s_seas = 0.1)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("sd_seas", "sd"))
  expect_identical(length(ans$sd_seas), 10L)
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2zero", {
  prior <- RW2(sd = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2zeroseasfix", {
  prior <- RW2_Seas(n_seas = 2, sd = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_rw2zeroseasvary", {
  prior <- RW2_Seas(n_seas = 2, s_seas = 0.1, sd = 0)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), c("sd_seas", "sd"))
  expect_identical(length(ans$sd_seas), 10L)
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_spline", {
  prior <- Sp()
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(names(ans), "sd")
  expect_identical(length(ans$sd), 10L)
})

test_that("'draw_vals_hyper' works with bage_prior_svd", {
  prior <- SVD(HMD)
  ans <- draw_vals_hyper(prior = prior,
                         n_sim = 10)
  expect_identical(ans, list())
})

test_that("'draw_vals_hyper' works with bage_prior_svd_ar", {
  prior <- SVD_AR1(HMD)
  prior_ar <- AR1()
  set.seed(0)
  ans_obtained <- draw_vals_hyper(prior = prior,
                                  n_sim = 10)
  set.seed(0)
  ans_expected <- draw_vals_hyper(prior = prior_ar,
                                  n_sim = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_hyper' works with bage_prior_svd_rwrandom", {
  prior <- SVD_RW(HMD, s = 0.5)
  prior_rw <- RW(s = 0.5)
  set.seed(0)
  ans_obtained <- draw_vals_hyper(prior = prior,
                                  n_sim = 10)
  set.seed(0)
  ans_expected <- draw_vals_hyper(prior = prior_rw,
                                  n_sim = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_hyper' works with bage_prior_svd_rwzero", {
  prior <- SVD_RW(HMD, s = 0.5, sd = 0)
  prior_rw <- RW(s = 0.5, sd = 0)
  set.seed(0)
  ans_obtained <- draw_vals_hyper(prior = prior,
                                  n_sim = 10)
  set.seed(0)
  ans_expected <- draw_vals_hyper(prior = prior_rw,
                                  n_sim = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_hyper' works with bage_prior_svd_rw2random", {
  prior <- SVD_RW2(HMD, s = 0.5)
  prior_rw <- RW2(s = 0.5)
  set.seed(0)
  ans_obtained <- draw_vals_hyper(prior = prior,
                                  n_sim = 10)
  set.seed(0)
  ans_expected <- draw_vals_hyper(prior = prior_rw,
                                  n_sim = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_hyper' works with bage_prior_svd_rw2zero", {
  prior <- SVD_RW2(HMD, s = 0.5, sd = 0)
  prior_rw <- RW2(s = 0.5, sd = 0)
  set.seed(0)
  ans_obtained <- draw_vals_hyper(prior = prior,
                                  n_sim = 10)
  set.seed(0)
  ans_expected <- draw_vals_hyper(prior = prior_rw,
                                  n_sim = 10)
  expect_identical(ans_obtained, ans_expected)
})


## 'draw_vals_hyperrand' ------------------------------------------------------

test_that("'draw_vals_hyperrand' works with bage_prior_lin - con is 'none'", {
  set.seed(0)
  prior <- Lin()
  matrix_along_by <- matrix(0:11, nr = 3)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = n_sim)
  expect_identical(names(ans), c("slope", "trend", "error"))
  expect_equal(ans$slope[1,], ans$trend[2,] - ans$trend[1,])
})

test_that("'draw_vals_hyperrand' works with bage_prior_lin - con is 'by'", {
  set.seed(0)
  prior <- Lin(con = 'by')
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = n_sim)
  expect_identical(names(ans), c("slope", "trend", "error"))
  expect_equal(ans$slope[1,], ans$trend[2,] - ans$trend[1,])
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$error[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_linar - con is 'none'", {
  set.seed(0)
  prior <- Lin_AR()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = n_sim)
  expect_identical(names(ans), c("slope", "trend", "error"))
  expect_equal(ans$slope[1,], ans$trend[2,] - ans$trend[1,])
})

test_that("'draw_vals_hyperrand' works with bage_prior_linar - con = 'by'", {
  set.seed(0)
  prior <- Lin_AR(con = 'by')
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = n_sim)
  expect_identical(names(ans), c("slope", "trend", "error"))
  expect_equal(ans$slope[1,], ans$trend[2,] - ans$trend[1,])
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$error[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwrandomseasfix - con is 'none'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s = 0.01)
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = 10)
  expect_identical(names(ans), c("trend", "season"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwrandomseasfix - con = 'by'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s = 0.01, con = 'by')
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = 10)
  expect_identical(names(ans), c("trend", "season"))
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$season[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwrandomseasvary - con is 'none'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s = 0.01, s_seas = 0.4)
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_time = var_time,
                                      n_sim = n_sim)
  expect_identical(names(ans), c("trend", "season"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwrandomseasvary - con is 'by'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s = 0.01, s_seas = 0.4, con = 'by')
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_time = var_time,
                                      n_sim = n_sim)
  expect_identical(names(ans), c("trend", "season"))
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$season[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwzeroseasfix - con is 'none'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s = 0.01, sd = 0)
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = 10)
  expect_identical(names(ans), c("trend", "season"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwzeroseasfix - con = 'by'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s = 0.01, sd = 0, con = 'by')
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = 10)
  expect_identical(names(ans), c("trend", "season"))
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$season[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwzeroseasvary - con is 'none'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s = 0.01, sd = 0, s_seas = 0.4)
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_time = var_time,
                                      n_sim = n_sim)
  expect_identical(names(ans), c("trend", "season"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rwzeroseasvary - con is 'by'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s = 0.01, sd = 0, s_seas = 0.4, con = 'by')
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_time = var_time,
                                      n_sim = n_sim)
  expect_identical(names(ans), c("trend", "season"))
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$season[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2randomseasfix - con is 'none'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s = 0.01)
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = 10)
  expect_identical(names(ans), c("trend", "season"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2randomseasfix - con = 'by'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s = 0.01, con = 'by')
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = 10)
  expect_identical(names(ans), c("trend", "season"))
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$season[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2randomseasvary - con is 'none'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s = 0.01, s_seas = 0.4)
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_time = var_time,
                                      n_sim = n_sim)
  expect_identical(names(ans), c("trend", "season"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2randomseasvary - con is 'by'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s = 0.01, s_seas = 0.4, con = 'by')
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_time = var_time,
                                      n_sim = n_sim)
  expect_identical(names(ans), c("trend", "season"))
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$season[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2zeroseasfix - con is 'none'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s = 0.01, sd = 0)
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = 10)
  expect_identical(names(ans), c("trend", "season"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2zeroseasfix - con = 'by'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s = 0.01, sd = 0, con = 'by')
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                             vals_hyper = vals_hyper,
                             dimnames_term = dimnames_term,
                             var_age = var_age,
                             var_time = var_time,
                             n_sim = 10)
  expect_identical(names(ans), c("trend", "season"))
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$season[,10], nr = 3)), rep(0, 3))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2zeroseasvary - con is 'none'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s = 0.01, sd = 0, s_seas = 0.4)
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_time = var_time,
                                      n_sim = n_sim)
  expect_identical(names(ans), c("trend", "season"))
})

test_that("'draw_vals_hyperrand' works with bage_prior_rw2zeroseasvary - con is 'by'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s = 0.01, sd = 0, s_seas = 0.4, con = 'by')
  dimnames_term <- list(time = 1:3, region = 1:4)
  var_time <- "time"
  var_age <- "age"
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  set.seed(1)
  ans <- draw_vals_hyperrand(prior = prior,
                                      vals_hyper = vals_hyper,
                                      dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_time = var_time,
                                      n_sim = n_sim)
  expect_identical(names(ans), c("trend", "season"))
  expect_equal(rowSums(matrix(ans$trend[,1], nr = 3)), rep(0, 3))
  expect_equal(rowSums(matrix(ans$season[,10], nr = 3)), rep(0, 3))
})


## 'draw_vals_spline' ---------------------------------------------------------

test_that("'draw_vals_spline' returns NULL with non-spline prior main effect", {
  prior <- AR()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = 0:80)
  var_time <- "time"
  var_age <- "age"
  levels_spline <- 1:81
  ans <- draw_vals_spline(prior = prior,
                          vals_hyper = vals_hyper,
                          dimnames_term = dimnames_term,
                          var_time = var_time,
                          var_age = var_age,
                          levels_spline = levels_spline,
                          n_sim = n_sim)
  expect_identical(ans, NULL)
})

test_that("'draw_vals_spline' works with 'bage_prior_spline' - con is 'none'", {
  set.seed(0)
  prior <- Sp(n_comp = 7)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = 0:80, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  levels_spline <- paste(paste0("comp", 1:7), rep(1:4, each = 7), sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_spline(prior = prior,
                                   vals_hyper = vals_hyper,
                                   dimnames_term = dimnames_term,
                                   var_time = var_time,
                                   var_age = var_age,
                                   levels_spline = levels_spline,
                                   n_sim = n_sim)
  set.seed(0)
  matrix_along_by <- matrix(0:27, nr = 7)
  ans_expected <- draw_vals_rw2(sd = vals_hyper$sd,
                                sd_init <- prior$specific$sd,
                                sd_slope = prior$specific$sd_slope,
                                matrix_along_by = matrix_along_by,
                                levels_effect = levels_spline)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_spline' works with 'bage_prior_spline' - con is 'by'", {
  set.seed(0)
  prior <- Sp(n_comp = 7, con = 'by')
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = 0:80, reg = 1:4)
  var_time <- "time"
  var_age <- "age"
  levels_spline <- paste(paste0("comp", 1:7), rep(paste0("reg", 1:3), each = 7), sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_spline(prior = prior,
                                   vals_hyper = vals_hyper,
                                   dimnames_term = dimnames_term,
                                   var_time = var_time,
                                   var_age = var_age,
                                   levels_spline = levels_spline,
                                   n_sim = n_sim)
  set.seed(0)
  matrix_along_by <- matrix(0:20, nr = 7)
  ans_expected <- draw_vals_rw2(sd = vals_hyper$sd,
                                sd_init <- prior$specific$sd,
                                sd_slope = prior$specific$sd_slope,
                                matrix_along_by = matrix_along_by,
                                levels_effect = levels_spline)
  expect_identical(ans_obtained, ans_expected)
})


## 'draw_vals_svd' ------------------------------------------------------------

test_that("'draw_vals_svd' returns NULL with non-SVD prior main effect", {
  prior <- AR()
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = 0:80)
  var_time <- "time"
  var_age <- "age"
  levels_svd <- 1:81
  ans <- draw_vals_svd(prior = prior,
                       vals_hyper = vals_hyper,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       levels_svd = levels_svd,
                       n_sim = n_sim)
  expect_identical(ans, NULL)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd'", {
  set.seed(0)
  prior <- SVD(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  levels_svd <- paste0("comp", 1:3)
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  ans_expected <- matrix(rnorm(30), nr = 3, nc = 10,
                         dimnames = list(levels_svd, NULL))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_ar' - con is 'none'", {
  set.seed(0)
  prior <- SVD_AR1(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(time = 2001:2003,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                             rep(2001:2003, each = 3),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- t(matrix(0:8, nr = 3))
  ans_expected <- draw_vals_ar(coef = vals_hyper$coef,
                               sd = vals_hyper$sd,
                               matrix_along_by = matrix_along_by_free,
                               levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_ar' - con is 'by'", {
  set.seed(0)
  prior <- SVD_AR1(HMD, con = 'by')
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(time = 2001:2003,
                        region = 1:3,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(2001:2003, each = 3),
                      rep(c("region1", "region2"), each = 9),
                      sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:17, dim = c(3, 3, 2)), perm = c(2, 1, 3)), nr = 3)
  ans_expected <- draw_vals_ar(coef = vals_hyper$coef,
                               sd = vals_hyper$sd,
                               matrix_along_by = matrix_along_by_free,
                               levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})


test_that("'draw_vals_svd' works with 'bage_prior_svd_rwrandom'", {
  set.seed(0)
  prior <- SVD_RW(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                             rep(2001:2005, each = 3),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- t(matrix(0:14, nr = 3))
  ans_expected <- draw_vals_rw(sd = vals_hyper$sd,
                               sd_init = prior$specific$sd,
                               matrix_along_by = matrix_along_by_free,
                               levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rwrandom' - con is 'none'", {
  set.seed(0)
  prior <- SVD_RW(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(reg = 1:3,
                        time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(1:3, each = 3),
                      rep(2001:2005, each = 9),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:44, c(3, 3, 5)), c(3, 1, 2)), nr = 5)
  ans_expected <- draw_vals_rw(sd = vals_hyper$sd,
                               sd_init = prior$specific$sd,
                               matrix_along_by = matrix_along_by_free,
                               levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rwrandom' - con is 'by'", {
  set.seed(0)
  prior <- SVD_RW(HMD, con = 'by')
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(reg = 1:3,
                        time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(c("reg1", "reg2"), each = 3),
                      rep(2001:2005, each = 6),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:29, c(3, 2, 5)), c(3, 1, 2)), nr = 5)
  ans_expected <- draw_vals_rw(sd = vals_hyper$sd,
                               sd_init = prior$specific$sd,
                               matrix_along_by = matrix_along_by_free,
                               levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rwzero'", {
  set.seed(0)
  prior <- SVD_RW(HMD, sd = 0)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                             rep(2001:2005, each = 3),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- t(matrix(0:14, nr = 3))
  ans_expected <- draw_vals_rw(sd = vals_hyper$sd,
                               sd_init = 0,
                               matrix_along_by = matrix_along_by_free,
                               levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rwzero' - con is 'none'", {
  set.seed(0)
  prior <- SVD_RW(HMD, sd = 0)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(reg = 1:3,
                        time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(1:3, each = 3),
                      rep(2001:2005, each = 9),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:44, c(3, 3, 5)), c(3, 1, 2)), nr = 5)
  ans_expected <- draw_vals_rw(sd = vals_hyper$sd,
                               sd_init = 0,
                               matrix_along_by = matrix_along_by_free,
                               levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rwzero' - con is 'by'", {
  set.seed(0)
  prior <- SVD_RW(HMD, sd = 0, con = 'by')
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(reg = 1:3,
                        time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(c("reg1", "reg2"), each = 3),
                      rep(2001:2005, each = 6),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:29, c(3, 2, 5)), c(3, 1, 2)), nr = 5)
  ans_expected <- draw_vals_rw(sd = vals_hyper$sd,
                               sd_init = 0,
                               matrix_along_by = matrix_along_by_free,
                               levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rw2random'", {
  set.seed(0)
  prior <- SVD_RW2(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                             rep(2001:2005, each = 3),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- t(matrix(0:14, nr = 3))
  ans_expected <- draw_vals_rw2(sd = vals_hyper$sd,
                                sd_init = prior$specific$sd,
                                sd_slope = prior$specific$sd_slope,
                                matrix_along_by = matrix_along_by_free,
                                levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rw2random' - con is 'none'", {
  set.seed(0)
  prior <- SVD_RW2(HMD)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(reg = 1:3,
                        time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(1:3, each = 3),
                      rep(2001:2005, each = 9),
                      sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:44, c(3, 3, 5)), c(3, 1, 2)), nr = 5)
  ans_expected <- draw_vals_rw2(sd = vals_hyper$sd,
                                sd_init = prior$specific$sd,
                                sd_slope = prior$specific$sd_slope,
                                matrix_along_by = matrix_along_by_free,
                                levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rw2random' - con is 'by'", {
  set.seed(0)
  prior <- SVD_RW2(HMD, con = 'by')
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(reg = 1:3,
                        time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(c("reg1", "reg2"), each = 3),
                      rep(2001:2005, each = 6),
                      sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:29, c(3, 2, 5)), c(3, 1, 2)), nr = 5)
  ans_expected <- draw_vals_rw2(sd = vals_hyper$sd,
                                sd_init = prior$specific$sd,
                                sd_slope = prior$specific$sd_slope,
                                matrix_along_by = matrix_along_by_free,
                                levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rw2zero'", {
  set.seed(0)
  prior <- SVD_RW2(HMD, sd = 0)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                             rep(2001:2005, each = 3),
                             sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- t(matrix(0:14, nr = 3))
  ans_expected <- draw_vals_rw2(sd = vals_hyper$sd,
                                sd_init = 0,
                                sd_slope = prior$specific$sd_slope,
                                matrix_along_by = matrix_along_by_free,
                                levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rw2zero' - con is 'none'", {
  set.seed(0)
  prior <- SVD_RW2(HMD, sd = 0)
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(reg = 1:3,
                        time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(1:3, each = 3),
                      rep(2001:2005, each = 9),
                      sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:44, c(3, 3, 5)), c(3, 1, 2)), nr = 5)
  ans_expected <- draw_vals_rw2(sd = vals_hyper$sd,
                                sd_init = 0,
                                sd_slope = prior$specific$sd_slope,
                                matrix_along_by = matrix_along_by_free,
                                levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'draw_vals_svd' works with 'bage_prior_svd_rw2zero' - con is 'by'", {
  set.seed(0)
  prior <- SVD_RW2(HMD, sd = 0, con = 'by')
  n_sim <- 10
  vals_hyper <- draw_vals_hyper(prior = prior,
                                n_sim = n_sim)
  dimnames_term <- list(reg = 1:3,
                        time = 2001:2005,
                        age = c(0:79, "80+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_svd <- paste(paste0("comp", 1:3),
                      rep(c("reg1", "reg2"), each = 3),
                      rep(2001:2005, each = 6),
                      sep = ".")
  set.seed(0)
  ans_obtained <- draw_vals_svd(prior = prior,
                                vals_hyper = vals_hyper,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                levels_svd = levels_svd,
                                n_sim = n_sim)
  set.seed(0)
  matrix_along_by_free <- matrix(aperm(array(0:29, c(3, 2, 5)), c(3, 1, 2)), nr = 5)
  ans_expected <- draw_vals_rw2(sd = vals_hyper$sd,
                                sd_init = 0,
                                sd_slope = prior$specific$sd_slope,
                                matrix_along_by = matrix_along_by_free,
                                levels_effect = levels_svd)
  expect_identical(ans_obtained, ans_expected)
})


## forecast_term ------------------------------------------------------------

test_that("'forecast_term' raises error with time-varying term for which method does not exist", {
  set.seed(0)
  prior <- Known(value = 1:5)
  dimnames_term <- list(year = 1:5)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- 6:11
  components <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = c(-2, -1, 0, 1, 2))
  expect_error(forecast_term(prior = prior,
                             dimnames_term = dimnames_term,
                             var_time = var_time,
                             var_age = var_age,
                             var_sexgender = var_sexgender,
                             components = components,
                             labels_forecast = labels_forecast),
               "Can't forecast term \"year\"")
})

test_that("'forecast_term' works with bage_prior_ar", {
  set.seed(0)
  prior <- AR(n_coef = 3)
  dimnames_term <- list(year = letters[1:5])
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- letters[6:11]
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("coef", "coef", "coef", "sd"),
                                                .fitted = rvec::runif_rvec(n = 4, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = letters[6:11])
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  coef <- components$.fitted[components$level == "coef"]
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(rev(coef) * components$.fitted[4 + 3:5]),
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(rev(coef) * c(components$.fitted[4 + 4:5],
                                                                  ans_expected$.fitted[1])),
                                              sd = sd)
  ans_expected$.fitted[3] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(rev(coef) * c(components$.fitted[4 + 5],
                                                                  ans_expected$.fitted[1:2])),
                                              sd = sd)
  ans_expected$.fitted[4] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(rev(coef) * ans_expected$.fitted[1:3]),
                                              sd = sd)
  ans_expected$.fitted[5] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(rev(coef) * ans_expected$.fitted[2:4]),
                                              sd = sd)
  ans_expected$.fitted[6] <- rvec::rnorm_rvec(n = 1,
                                              mean = sum(rev(coef) * ans_expected$.fitted[3:5]),
                                              sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_ar - con is 'by'", {
  set.seed(0)
  prior <- AR(con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(
    tibble::tibble(term = "year:reg",
                   component = "hyper",
                   level = c("coef", "coef", "coef", "sd"),
                   .fitted = rvec::runif_rvec(n = 4, n_draw = 10)),
    tibble::tibble(term = "year:reg", component = "effect",
                   level = paste(2001:2005, rep(1:2, each = 5), sep = "."),
                   .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10))
  )
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[1:6], -ans$.fitted[7:12])  
})

test_that("'forecast_term' works with bage_prior_lin - con is 'none'", {
  set.seed(0)
  prior <- Lin()
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = c("slope", "slope",
                                                          "sd"),
                                                .fitted = rvec::runif_rvec(n = 3, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  labels_forecast <- 2006:2011
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = paste(2006:2011,
                                               rep(1:2, each = 6),
                                               sep = "."))
  slope <- components$.fitted[components$level == "slope"]
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  intercept <- -0.5 * 6 * slope
  ans_expected$.fitted <- c(rvec::rnorm_rvec(n = 6,
                                             mean = intercept[1] + (6:11) * slope[1],
                                             sd = sd),
                            rvec::rnorm_rvec(n = 6,
                                             mean = intercept[2] + (6:11) * slope[2],
                                             sd = sd))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_lin - con is 'by'", {
  set.seed(0)
  prior <- Lin(con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = c("slope", "slope",
                                                          "sd"),
                                                .fitted = rvec::runif_rvec(n = 3, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  labels_forecast <- 2006:2011
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[4], -ans$.fitted[10])
})

test_that("'forecast_term' works with bage_prior_linar - n_by = 1", {
  set.seed(0)
  prior <- Lin_AR()
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("slope", "sd", "coef1", "coef2"),
                                                .fitted = rvec::runif_rvec(n = 4, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  labels_forecast <- 2006:2011
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  effect <- components$.fitted[components$component == "effect"]
  slope <- components$.fitted[components$level == "slope"]
  intercept <- -0.5 * 6 * slope
  sd <- components$.fitted[components$level == "sd"]
  coef <- components$.fitted[components$level %in% c("coef1", "coef2")]
  error_forecast <- rep(effect[[1]], 6)
  set.seed(1)
  trend_forecast <- intercept + slope * (6:11)
  error_forecast[1] <- rvec::rnorm_rvec(n = 1,
                               mean = sum(rev(coef) * (components$.fitted[4 + 4:5] -
                                                    (intercept + slope * (4:5)))),
                               sd = sd)
  error_forecast[2] <- rvec::rnorm_rvec(n = 1,
                               mean = sum(rev(coef) * c(components$.fitted[4 + 5] - (intercept + slope * 5),
                                                   error_forecast[1])),
                               sd = sd)
  error_forecast[3] <- rvec::rnorm_rvec(n = 1,
                               mean = sum(rev(coef) * (error_forecast[1:2])),
                               sd = sd)
  error_forecast[4] <- rvec::rnorm_rvec(n = 1,
                               mean = sum(rev(coef) * (error_forecast[2:3])),
                               sd = sd)
  error_forecast[5] <- rvec::rnorm_rvec(n = 1,
                               mean = sum(rev(coef) * (error_forecast[3:4])),
                               sd = sd)
  error_forecast[6] <- rvec::rnorm_rvec(n = 1,
                               mean = sum(rev(coef) * (error_forecast[4:5])),
                               sd = sd)
  effect_forecast <- trend_forecast + error_forecast
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "error"), each = 6),
                                 level = rep(as.character(2006:2011), times = 3),
                                 .fitted = c(effect_forecast, trend_forecast, error_forecast))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_linar - con is 'by'", {
  set.seed(0)
  prior <- Lin_AR1(con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = c("slope", "slope", "sd", "coef"),
                                                .fitted = rvec::runif_rvec(n = 4, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  labels_forecast <- 2006:2011
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[4], -ans$.fitted[10])
})


test_that("'forecast_term' works with bage_prior_linex - con is 'none'", {
  set.seed(0)
  prior <- Lin(s = 0)
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  labels_forecast <- 2006:2011
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = paste(2006:2011,
                                               rep(1:2, each = 6),
                                               sep = "."))
  effect <- components$.fitted
  slope <- c(effect[2] - effect[1], effect[7] - effect[6])
  intercept <- -0.5 * 6 * slope
  ans_expected$.fitted <- c(intercept[1] + (6:11) * slope[1],
                            intercept[2] + (6:11) * slope[2])
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_linex - con is 'by'", {
  set.seed(0)
  prior <- Lin(con = 'by', s = 0)
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = c("slope", "slope",
                                                          "sd"),
                                                .fitted = rvec::runif_rvec(n = 3, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  labels_forecast <- 2006:2011
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[4], -ans$.fitted[10])
})

test_that("'forecast_term' works with bage_prior_norm", {
  set.seed(0)
  prior <- N()
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  labels_forecast <- as.character(2006:2011)
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = as.character(2006:2011))
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, mean = 0, sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_normfixed", {
  set.seed(0)
  prior <- NFix()
  components <- tibble::tibble(term = "year",
                               component = "effect",
                               level = letters[1:5],
                               .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10))
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = as.character(2006:2011))
  set.seed(1)
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, mean = 0, sd = 1, n_draw = 10)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwrandom - n_by = 1", {
  set.seed(0)
  prior <- RW()
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = as.character(2006:2011))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[6],
                                              sd = sd)
  for (i in 2:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwrandom - n_by = 2, con is 'none'", {
  set.seed(0)
  prior <- RW()
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = paste(2006:2011,
                                               rep(1:2, each = 6),
                                               sep = "."))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[1 + 5],
                                              sd = sd)
  for (i in 2:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[1 + 10],
                                              sd = sd)
  for (i in 8:12)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwrandom - n_by = 2, con is 'by'", {
  set.seed(0)
  prior <- RW(con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[6], -ans$.fitted[12])
})

test_that("'forecast_term' works with bage_prior_rwrandomseasfix", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "season",
                                                level = as.character(2001:2005),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[2:6] - components$.fitted[7:11]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = as.character(2001:2005),
                                                .fitted = trend))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "season"), each = 6),
                                 level = rep(as.character(2006:2011), 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw_est <- components$.fitted[12:16]
  rw_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[[1]]
  set.seed(1)
  rw_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = rw_est[5], sd = sd)
  for (i in 2:6)
    rw_forecast[i] <- rvec::rnorm_rvec(n = 1, mean = rw_forecast[i-1], sd = sd)
  ans_expected$.fitted[13] <- components$.fitted[8]
  ans_expected$.fitted[14] <- components$.fitted[7]
  ans_expected$.fitted[15] <- components$.fitted[8]
  ans_expected$.fitted[16] <- components$.fitted[7]
  ans_expected$.fitted[17] <- components$.fitted[8]
  ans_expected$.fitted[18] <- components$.fitted[7]
  effect_forecast <- ans_expected$.fitted[13:18] + rw_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwrandomseasfix - con is 'by'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        sex = c("f", "m"))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:sex",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "season",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 10)))
  trend <- components$.fitted[2:11] - components$.fitted[12:21]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year:sex",
                                                component = "trend",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = trend))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[2], -ans$.fitted[8])
  expect_equal(ans$.fitted[15], -ans$.fitted[21])
  expect_equal(ans$.fitted[29], -ans$.fitted[35])
})

test_that("'forecast_term' works with bage_prior_rwrandomseasvary", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s_seas = 1)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "season",
                                                level = as.character(2001:2005),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[3:7] - components$.fitted[8:12]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = as.character(2001:2005),
                                                .fitted = trend))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "season"), each = 6),
                                 level = rep(as.character(2006:2011), 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw_est <- components$.fitted[13:17]
  rw_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd_seas <- components$.fitted[[1]]
  sd <- components$.fitted[[2]]
  set.seed(1)
  rw_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = rw_est[5], sd = sd)
  for (i in 2:6)
    rw_forecast[i] <- rvec::rnorm_rvec(n = 1, mean = rw_forecast[i-1], sd = sd)
  ans_expected$.fitted[13] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[11],
                                               sd = sd_seas)
  ans_expected$.fitted[14] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[12],
                                               sd = sd_seas)
  for (i in 3:6)
    ans_expected$.fitted[12 + i] <- rvec::rnorm_rvec(n = 1,
                                                     ans_expected$.fitted[10 + i],
                                                     sd = sd_seas)
  effect_forecast <- ans_expected$.fitted[13:18] + rw_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwrandomseasvary - con is 'by'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s_seas = 1, con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        sex = c("f", "m"))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:sex",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "season",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 10)))
  trend <- components$.fitted[3:12] - components$.fitted[13:22]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year:sex",
                                                component = "trend",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = trend))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[2], -ans$.fitted[8])
  expect_equal(ans$.fitted[15], -ans$.fitted[21])
  expect_equal(ans$.fitted[29], -ans$.fitted[35])
})



test_that("'forecast_term' works with bage_prior_rwzero - n_by = 1", {
  set.seed(0)
  prior <- RW(sd = 0)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = as.character(2006:2011))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[6],
                                              sd = sd)
  for (i in 2:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwzero - n_by = 2, con is 'none'", {
  set.seed(0)
  prior <- RW(sd = 0)
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = paste(2006:2011,
                                               rep(1:2, each = 6),
                                               sep = "."))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[1 + 5],
                                              sd = sd)
  for (i in 2:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = components$.fitted[1 + 10],
                                              sd = sd)
  for (i in 8:12)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = ans_expected$.fitted[i-1],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwzero - n_by = 2, con is 'by'", {
  set.seed(0)
  prior <- RW(con = 'by', sd = 0)
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[6], -ans$.fitted[12])
})

test_that("'forecast_term' works with bage_prior_rwzeroseasfix", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, sd = 0)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = letters[1:5],
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "season",
                                                level = as.character(2001:2005),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[2:6] - components$.fitted[7:11]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = as.character(2001:2005),
                                                .fitted = trend))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "season"), each = 6),
                                 level = rep(as.character(2006:2011), 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw_est <- components$.fitted[12:16]
  rw_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[[1]]
  set.seed(1)
  rw_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = rw_est[5], sd = sd)
  for (i in 2:6)
    rw_forecast[i] <- rvec::rnorm_rvec(n = 1, mean = rw_forecast[i-1], sd = sd)
  ans_expected$.fitted[13] <- components$.fitted[8]
  ans_expected$.fitted[14] <- components$.fitted[7]
  ans_expected$.fitted[15] <- components$.fitted[8]
  ans_expected$.fitted[16] <- components$.fitted[7]
  ans_expected$.fitted[17] <- components$.fitted[8]
  ans_expected$.fitted[18] <- components$.fitted[7]
  effect_forecast <- ans_expected$.fitted[13:18] + rw_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwzeroseasfix - con is 'by'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, con = 'by', sd = 0)
  dimnames_term <- list(year = 2001:2005,
                        sex = c("f", "m"))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:sex",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "season",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 10)))
  trend <- components$.fitted[2:11] - components$.fitted[12:21]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year:sex",
                                                component = "trend",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = trend))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[2], -ans$.fitted[8])
  expect_equal(ans$.fitted[15], -ans$.fitted[21])
  expect_equal(ans$.fitted[29], -ans$.fitted[35])
})

test_that("'forecast_term' works with bage_prior_rwzeroseasvary", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s_seas = 1, sd = 0)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "season",
                                                level = as.character(2001:2005),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[3:7] - components$.fitted[8:12]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = as.character(2001:2005),
                                                .fitted = trend))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "season"), each = 6),
                                 level = rep(as.character(2006:2011), 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw_est <- components$.fitted[13:17]
  rw_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd_seas <- components$.fitted[[1]]
  sd <- components$.fitted[[2]]
  set.seed(1)
  rw_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = rw_est[5], sd = sd)
  for (i in 2:6)
    rw_forecast[i] <- rvec::rnorm_rvec(n = 1, mean = rw_forecast[i-1], sd = sd)
  ans_expected$.fitted[13] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[11],
                                               sd = sd_seas)
  ans_expected$.fitted[14] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[12],
                                               sd = sd_seas)
  for (i in 3:6)
    ans_expected$.fitted[12 + i] <- rvec::rnorm_rvec(n = 1,
                                                     ans_expected$.fitted[10 + i],
                                                     sd = sd_seas)
  effect_forecast <- ans_expected$.fitted[13:18] + rw_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rwzeroseasvary - con is 'by'", {
  set.seed(0)
  prior <- RW_Seas(n_seas = 2, s_seas = 1, con = 'by', sd = 0)
  dimnames_term <- list(year = 2001:2005,
                        sex = c("f", "m"))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:sex",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "season",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 10)))
  trend <- components$.fitted[3:12] - components$.fitted[13:22]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year:sex",
                                                component = "trend",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = trend))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[2], -ans$.fitted[8])
  expect_equal(ans$.fitted[15], -ans$.fitted[21])
  expect_equal(ans$.fitted[29], -ans$.fitted[35])
})

test_that("'forecast_term' works with bage_prior_rw2random - n_by = 1", {
  set.seed(0)
  prior <- RW2()
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = as.character(2006:2011))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[6] - components$.fitted[5],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[1] -
                                                components$.fitted[6],
                                              sd = sd)
  for (i in 3:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2random - n_by = 2, con is 'none'", {
  set.seed(0)
  prior <- RW2()
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = paste(2006:2011,
                                               rep(1:2, each = 6),
                                               sep = "."))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[6] - components$.fitted[5],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[1] -
                                                components$.fitted[6],
                                              sd = sd)
  for (i in 3:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[11] -
                                                components$.fitted[10],
                                              sd = sd)
  ans_expected$.fitted[8] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[7] -
                                                components$.fitted[11],
                                              sd = sd)
  for (i in 9:12)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2random - n_by = 2, con is 'by'", {
  set.seed(0)
  prior <- RW2(con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[3], -ans$.fitted[9])  
})

test_that("'forecast_term' works with bage_prior_rw2randomseasfix", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "season",
                                                level = as.character(2001:2005),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[2:6] - components$.fitted[7:11]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = as.character(2001:2005),
                                                .fitted = trend))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "season"), each = 6),
                                 level = rep(as.character(2006:2011), 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw2_est <- components$.fitted[12:16]
  rw2_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[[1]]
  set.seed(1)
  rw2_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_est[5] - rw2_est[4], sd = sd)
  rw2_forecast[2] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_forecast[1] - rw2_est[5], sd = sd)
  for (i in 3:6)
    rw2_forecast[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * rw2_forecast[i-1] - rw2_forecast[i-2],
                                        sd = sd)
  ans_expected$.fitted[13] <- components$.fitted[8]
  ans_expected$.fitted[14] <- components$.fitted[7]
  ans_expected$.fitted[15] <- components$.fitted[8]
  ans_expected$.fitted[16] <- components$.fitted[7]
  ans_expected$.fitted[17] <- components$.fitted[8]
  ans_expected$.fitted[18] <- components$.fitted[7]
  effect_forecast <- ans_expected$.fitted[13:18] + rw2_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw2_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2randomseasfix - con is 'by'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        sex = c("f", "m"))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:sex",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "season",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 10)))
  trend <- components$.fitted[2:11] - components$.fitted[12:21]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year:sex",
                                                component = "trend",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = trend))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[2], -ans$.fitted[8])
  expect_equal(ans$.fitted[15], -ans$.fitted[21])
  expect_equal(ans$.fitted[29], -ans$.fitted[35])
})

test_that("'forecast_term' works with bage_prior_rw2randomseasvary", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s_seas = 1)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "season",
                                                level = as.character(2001:2005),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[3:7] - components$.fitted[8:12]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = as.character(2001:2005),
                                                .fitted = trend))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "season"), each = 6),
                                 level = rep(as.character(2006:2011), 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw2_est <- components$.fitted[13:17]
  rw2_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd_seas <- components$.fitted[[1]]
  sd <- components$.fitted[[2]]
  set.seed(1)
  rw2_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_est[5] - rw2_est[4], sd = sd)
  rw2_forecast[2] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_forecast[1] - rw2_est[5], sd = sd)
  for (i in 3:6)
    rw2_forecast[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * rw2_forecast[i-1] - rw2_forecast[i-2],
                                        sd = sd)
  ans_expected$.fitted[13] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[11],
                                               sd = sd_seas)
  ans_expected$.fitted[14] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[12],
                                               sd = sd_seas)
  for (i in 3:6)
    ans_expected$.fitted[12 + i] <- rvec::rnorm_rvec(n = 1,
                                                     ans_expected$.fitted[10 + i],
                                                     sd = sd_seas)
  effect_forecast <- ans_expected$.fitted[13:18] + rw2_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw2_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2randomseasvary - con is 'by'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s_seas = 0.1, con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        sex = c("f", "m"))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:sex",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "season",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 10)))
  trend <- components$.fitted[3:12] - components$.fitted[13:22]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year:sex",
                                                component = "trend",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = trend))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[2], -ans$.fitted[8])
  expect_equal(ans$.fitted[15], -ans$.fitted[21])
  expect_equal(ans$.fitted[29], -ans$.fitted[35])
})

test_that("'forecast_term' works with bage_prior_rw2zero - n_by = 1", {
  set.seed(0)
  prior <- RW2(sd = 0)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = "effect",
                                 level = as.character(2006:2011))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[6] - components$.fitted[5],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[1] -
                                                components$.fitted[6],
                                              sd = sd)
  for (i in 3:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2zero - n_by = 2, con is 'none'", {
  set.seed(0)
  prior <- RW2(sd = 0)
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year:reg",
                                 component = "effect",
                                 level = paste(2006:2011,
                                               rep(1:2, each = 6),
                                               sep = "."))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 12, n_draw = 10)
  sd <- components$.fitted[components$level == "sd"]
  set.seed(1)
  ans_expected$.fitted[1] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[6] - components$.fitted[5],
                                              sd = sd)
  ans_expected$.fitted[2] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[1] -
                                                components$.fitted[6],
                                              sd = sd)
  for (i in 3:6)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  ans_expected$.fitted[7] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * components$.fitted[11] -
                                                components$.fitted[10],
                                              sd = sd)
  ans_expected$.fitted[8] <- rvec::rnorm_rvec(n = 1,
                                              mean = 2 * ans_expected$.fitted[7] -
                                                components$.fitted[11],
                                              sd = sd)
  for (i in 9:12)
    ans_expected$.fitted[i] <- rvec::rnorm_rvec(n = 1,
                                                mean = 2 * ans_expected$.fitted[i-1] -
                                                  ans_expected$.fitted[i-2],
                                                sd = sd)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2zero - n_by = 2, con is 'by'", {
  set.seed(0)
  prior <- RW2(sd = 0, con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        reg = 1:2)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:reg",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:reg",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(1:2, each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[3], -ans$.fitted[9])  
})

test_that("'forecast_term' works with bage_prior_rw2zeroseasfix", {
  set.seed(0)
  prior <- RW2_Seas(sd = 0, n_seas = 2)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "season",
                                                level = as.character(2001:2005),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[2:6] - components$.fitted[7:11]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = as.character(2001:2005),
                                                .fitted = trend))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "season"), each = 6),
                                 level = rep(as.character(2006:2011), 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw2_est <- components$.fitted[12:16]
  rw2_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd <- components$.fitted[[1]]
  set.seed(1)
  rw2_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_est[5] - rw2_est[4], sd = sd)
  rw2_forecast[2] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_forecast[1] - rw2_est[5], sd = sd)
  for (i in 3:6)
    rw2_forecast[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * rw2_forecast[i-1] - rw2_forecast[i-2],
                                        sd = sd)
  ans_expected$.fitted[13] <- components$.fitted[8]
  ans_expected$.fitted[14] <- components$.fitted[7]
  ans_expected$.fitted[15] <- components$.fitted[8]
  ans_expected$.fitted[16] <- components$.fitted[7]
  ans_expected$.fitted[17] <- components$.fitted[8]
  ans_expected$.fitted[18] <- components$.fitted[7]
  effect_forecast <- ans_expected$.fitted[13:18] + rw2_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw2_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2zeroseasfix - con is 'by'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, sd = 0, con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        sex = c("f", "m"))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:sex",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "season",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 10)))
  trend <- components$.fitted[2:11] - components$.fitted[12:21]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year:sex",
                                                component = "trend",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = trend))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[2], -ans$.fitted[8])
  expect_equal(ans$.fitted[15], -ans$.fitted[21])
  expect_equal(ans$.fitted[29], -ans$.fitted[35])
})

test_that("'forecast_term' works with bage_prior_rw2zeroseasvary", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s_seas = 1, sd = 0)
  dimnames_term <- list(year = 2001:2005)
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "effect",
                                                level = as.character(2001:2005),
                                                .fitted = rvec::rnorm_rvec(n = 5, n_draw = 10)),
                                 tibble::tibble(term = "year",
                                                component = "season",
                                                level = as.character(2001:2005),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 5)))
  trend <- components$.fitted[3:7] - components$.fitted[8:12]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year",
                                                component = "trend",
                                                level = as.character(2001:2005),
                                                .fitted = trend))
  set.seed(1)
  ans_obtained <- forecast_term(prior = prior,
                                dimnames_term = dimnames_term,
                                var_time = var_time,
                                var_age = var_age,
                                var_sexgender = var_sexgender,
                                components = components,
                                labels_forecast = labels_forecast)
  ans_expected <- tibble::tibble(term = "year",
                                 component = rep(c("effect", "trend", "season"), each = 6),
                                 level = rep(as.character(2006:2011), 3))
  ans_expected$.fitted <- rvec::rnorm_rvec(n = 18, n_draw = 10)
  rw2_est <- components$.fitted[13:17]
  rw2_forecast <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  sd_seas <- components$.fitted[[1]]
  sd <- components$.fitted[[2]]
  set.seed(1)
  rw2_forecast[1] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_est[5] - rw2_est[4], sd = sd)
  rw2_forecast[2] <- rvec::rnorm_rvec(n = 1, mean = 2 * rw2_forecast[1] - rw2_est[5], sd = sd)
  for (i in 3:6)
    rw2_forecast[i] <- rvec::rnorm_rvec(n = 1,
                                        mean = 2 * rw2_forecast[i-1] - rw2_forecast[i-2],
                                        sd = sd)
  ans_expected$.fitted[13] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[11],
                                               sd = sd_seas)
  ans_expected$.fitted[14] <- rvec::rnorm_rvec(n = 1,
                                               components$.fitted[12],
                                               sd = sd_seas)
  for (i in 3:6)
    ans_expected$.fitted[12 + i] <- rvec::rnorm_rvec(n = 1,
                                                     ans_expected$.fitted[10 + i],
                                                     sd = sd_seas)
  effect_forecast <- ans_expected$.fitted[13:18] + rw2_forecast
  ans_expected$.fitted[1:6] <- effect_forecast
  ans_expected$.fitted[7:12] <- rw2_forecast
  expect_equal(ans_obtained, ans_expected)
})

test_that("'forecast_term' works with bage_prior_rw2zeroseasvary - con is 'by'", {
  set.seed(0)
  prior <- RW2_Seas(n_seas = 2, s_seas = 0.1, sd = 0, con = 'by')
  dimnames_term <- list(year = 2001:2005,
                        sex = c("f", "m"))
  var_time <- "year"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- as.character(2006:2011)
  components <- vctrs::vec_rbind(tibble::tibble(term = "year:sex",
                                                component = "hyper",
                                                level = c("sd_seas", "sd"),
                                                .fitted = rvec::runif_rvec(n = 2, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "effect",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 10, n_draw = 10)),
                                 tibble::tibble(term = "year:sex",
                                                component = "season",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = rep(rvec::rnorm_rvec(n = 2, n_draw = 10),
                                                              length.out = 10)))
  trend <- components$.fitted[3:12] - components$.fitted[13:22]
  components <- vctrs::vec_rbind(components,
                                 tibble::tibble(term = "year:sex",
                                                component = "trend",
                                                level = paste(2001:2005,
                                                              rep(c("f", "m"), each = 5),
                                                              sep = "."),
                                                .fitted = trend))
  set.seed(1)
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(ans$.fitted[2], -ans$.fitted[8])
  expect_equal(ans$.fitted[15], -ans$.fitted[21])
  expect_equal(ans$.fitted[29], -ans$.fitted[35])
})


test_that("'forecast_term' works with bage_prior_svd_ar - con is 'none'", {
  set.seed(0)
  prior <- SVD_AR(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- 2011:2015
  components <- vctrs::vec_rbind(tibble::tibble(term = "age:time",
                                                component = "hyper",
                                                level = c("coef", "coef", "sd"),
                                                .fitted = rvec::runif_rvec(n = 3, n_draw = 10)),
                                 tibble::tibble(term = "age:time",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(2001:2010, each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 30, n_draw = 10)))
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  expect_setequal(ans$component, c("svd", "effect"))
  expect_identical(nrow(ans), 15L + length(dimnames_term$age) * 5L)
})

test_that("'forecast_term' works with bage_prior_svd_ar - con is 'by'", {
  set.seed(0)
  prior <- SVD_AR(HMD, con = 'by')
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- 2011:2015
  components <- vctrs::vec_rbind(tibble::tibble(term = "age:time",
                                                component = "hyper",
                                                level = c("coef", "coef", "sd"),
                                                .fitted = rvec::runif_rvec(n = 3, n_draw = 10)),
                                 tibble::tibble(term = "age:time",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(2001:2010, each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 30, n_draw = 10)))
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_equal(rvec::draws_mean(sum(ans$.fitted[57:70])), 0)
})

test_that("'forecast_term' works with bage_prior_svd_rwrandom - con is 'none'", {
  set.seed(0)
  prior <- SVD_RW(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- 2011:2015
  components <- vctrs::vec_rbind(tibble::tibble(term = "age:time",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "age:time",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(2001:2010, each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 30, n_draw = 10)))
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  expect_setequal(ans$component, c("svd", "effect"))
  expect_identical(nrow(ans), 15L + length(dimnames_term$age) * 5L)
})

test_that("'forecast_term' works with bage_prior_svd_rwzero - con is 'none'", {
  set.seed(0)
  prior <- SVD_RW(HMD, sd = 0)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- 2011:2015
  components <- vctrs::vec_rbind(tibble::tibble(term = "age:time",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "age:time",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(2001:2010, each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 30, n_draw = 10)))
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  expect_setequal(ans$component, c("svd", "effect"))
  expect_identical(nrow(ans), 15L + length(dimnames_term$age) * 5L)
})

test_that("'forecast_term' works with bage_prior_svd_rwrandom - con is 'by'", {
  set.seed(0)
  prior <- SVD_RW(HMD, con = 'by')
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- 2011:2015
  components <- vctrs::vec_rbind(tibble::tibble(term = "age:time",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "age:time",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(2001:2010, each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 30, n_draw = 10)))
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  expect_setequal(ans$component, c("svd", "effect"))
  expect_identical(nrow(ans), 15L + length(dimnames_term$age) * 5L)
  expect_equal(rvec::draws_mean(sum(ans$.fitted[57:70])), 0)
})

test_that("'forecast_term' works with bage_prior_svd_rw2random - con is 'by'", {
  set.seed(0)
  prior <- SVD_RW2(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- 2011:2015
  components <- vctrs::vec_rbind(tibble::tibble(term = "age:time",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "age:time",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(2001:2010, each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 30, n_draw = 10)))
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  expect_setequal(ans$component, c("svd", "effect"))
  expect_identical(nrow(ans), 15L + length(dimnames_term$age) * 5L)
})

test_that("'forecast_term' works with bage_prior_svd_rw2zero - con is 'by'", {
  set.seed(0)
  prior <- SVD_RW2(HMD, con = 'by', sd = 0)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  labels_forecast <- 2011:2015
  components <- vctrs::vec_rbind(tibble::tibble(term = "age:time",
                                                component = "hyper",
                                                level = "sd",
                                                .fitted = rvec::runif_rvec(n = 1, n_draw = 10)),
                                 tibble::tibble(term = "age:time",
                                                component = "svd",
                                                level = paste(paste0("comp", 1:3),
                                                              rep(2001:2010, each = 3),
                                                              sep = "."),
                                                .fitted = rvec::rnorm_rvec(n = 30, n_draw = 10)))
  ans <- forecast_term(prior = prior,
                       dimnames_term = dimnames_term,
                       var_time = var_time,
                       var_age = var_age,
                       var_sexgender = var_sexgender,
                       components = components,
                       labels_forecast = labels_forecast)
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  expect_setequal(ans$component, c("svd", "effect"))
  expect_identical(nrow(ans), 15L + length(dimnames_term$age) * 5L)
  expect_equal(rvec::draws_mean(sum(ans$.fitted[57:70])), 0)
})
                   

## 'generate' -----------------------------------------------------------------

test_that("'generate' works with bage_prior_ar, n_by = 1", {
  x <- AR()
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  coef <- draw_vals_coef(x, n_sim = n_draw)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  ans_expected <- draw_vals_ar(coef = coef,
                               sd = sd,
                               matrix_along_by = matrix(0:19, nr = 20),
                               levels_effect = seq_len(20))
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_ar, n_by = 2", {
  x <- AR()
  set.seed(0)
  n_along <- 10
  n_draw <- 25
  n_by <- 2
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  coef <- draw_vals_coef(x, n_sim = n_draw)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(0:19, nc = 2)
  ans_expected <- draw_vals_ar(coef = coef,
                               sd = sd,
                               matrix_along_by = matrix(0:19, nr = n_along),
                               levels_effect = paste(rep(1:2, each = n_along),
                                                     seq_len(n_along),
                                                     sep = "."))
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = 10), times = 25)),
                         along = rep(seq_len(10), times = 2 * n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_ar, n_by = 2, con = 'by'", {
  x <- AR(con = "by")
  set.seed(0)
  n_along <- 10
  n_draw <- 25
  n_by <- 2
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  coef <- draw_vals_coef(x, n_sim = n_draw)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(0:19, nc = 2)
  ans_expected <- draw_vals_ar(coef = coef,
                               sd = sd,
                               matrix_along_by = matrix(0:19, nr = n_along),
                               levels_effect = seq_len(n_along * n_by))
  ans_expected <- array(ans_expected, c(n_along, n_by, n_draw))
  means <- apply(ans_expected, c(1, 3), mean)
  ans_expected[,1,] <- ans_expected[,1,] - means
  ans_expected[,2,] <- ans_expected[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = 10), times = 25)),
                         along = rep(seq_len(10), times = 2 * n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_known", {
  x <- Known(values = 1:5)
  set.seed(0)
  n_element <- 20
  n_draw <- 25
  ans_obtained <- generate(x, n_element = n_element, n_draw = n_draw)
  set.seed(0)
  ans_expected <- rep(1:5, times = 25)
  draw <- rep(seq_len(n_draw), each = 5)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         element = rep(seq_len(5), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
  expect_message(generate(x, n_element = 3, n_draw = n_draw),
                 "Non-default value of `n_element` ignored with \"Known\\(\\)\" prior.")
})

test_that("'generate' works with bage_prior_lin, n_by = 1", {
  x <- Lin()
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  slope <- rnorm(n = n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_draw)
  lin <- lin * rep(slope, each = n_along)
  error <- matrix(rnorm(n_along * n_draw, sd = rep(sd, each = n_along)),
                  nrow = n_along)
  value <- lin + error
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_lin, n_by = 2", {
  x <- Lin()
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  slope <- rnorm(n = n_by * n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_by * n_draw)
  lin <- lin * rep(slope, each = n_along)
  error <- matrix(rnorm(n_along * n_by * n_draw, sd = rep(sd, each = n_by * n_along)), nrow = n_along)
  value <- lin + error
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  by <- factor(rep(rep(paste("By", 1:2), each = 10), times = n_draw))
  ans_expected <- tibble(draw = draw,
                         by = by,
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_lin, n_by = 2, con = 'by'", {
  x <- Lin(con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  slope <- rnorm(n = n_by * n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_by * n_draw)
  lin <- lin * rep(slope, each = n_along)
  error <- matrix(rnorm(n_along * n_by * n_draw, sd = rep(sd, each = n_by * n_along)), nrow = n_along)
  value <- lin + error
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  by <- factor(rep(rep(paste("By", 1:2), each = 10), times = n_draw))
  ans_expected <- tibble(draw = draw,
                         by = by,
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_linar, n_by = 1", {
  x <- Lin_AR()
  set.seed(0)
  n_along <- 20
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_draw = n_draw)
  set.seed(0)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_draw)
  lin <- lin * rep(rnorm(n = n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope),
                   each = n_along)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  coef <- draw_vals_coef(x, n_sim = n_draw)
  error <- draw_vals_ar_inner(n = n_along, coef = coef, sd = sd)
  value <- lin + error
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_linar, n_by = 2", {
  x <- Lin_AR()
  set.seed(0)
  n_along <- 10
  n_draw <- 25
  n_by <- 2L
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  slope <- rnorm(n = n_by * n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  coef <- draw_vals_coef(x, n_sim = n_draw)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_by * n_draw)
  lin <- lin * rep(slope, each = n_along)
  error <- draw_vals_ar_inner(n = n_along,
                              coef = coef[,rep(seq_len(n_draw), each = 2)],
                              sd = rep(sd, each = 2))
  value <- lin + error
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_linar, n_by = 2, con = 'by'", {
  x <- Lin_AR(con = "by")
  set.seed(0)
  n_along <- 10
  n_draw <- 25
  n_by <- 2L
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  slope <- rnorm(n = n_by * n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  coef <- draw_vals_coef(x, n_sim = n_draw)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_by * n_draw)
  lin <- lin * rep(slope, each = n_along)
  error <- draw_vals_ar_inner(n = n_along,
                              coef = coef[,rep(seq_len(n_draw), each = 2)],
                              sd = rep(sd, each = 2))
  value <- lin + error
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_linex", {
  x <- Lin(s = 0)
  set.seed(0)
  n_along <- 20
  n_draw <- 25
  n_by <- 1
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_draw)
  lin <- lin * rep(rnorm(n = n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope),
                   each = n_along)
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(lin))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_linex, n_by = 2", {
  x <- Lin(s = 0)
  set.seed(0)
  n_along <- 10
  n_draw <- 25
  n_by <- 2
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  slope <- rnorm(n = n_by * n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_by * n_draw)
  lin <- lin * rep(slope, each = n_along)
  value <- as.numeric(lin)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_linex, n_by = 2, con = 'by'", {
  x <- Lin(s = 0, con = "by")
  set.seed(0)
  n_along <- 10
  n_draw <- 25
  n_by <- 2
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  slope <- rnorm(n = n_by * n_draw, mean = x$specific$mean_slope, sd = x$specific$sd_slope)
  lin <- matrix(1:n_along - 0.5 * (n_along + 1), nrow = n_along, ncol = n_by * n_draw)
  lin <- lin * rep(slope, each = n_along)
  value <- as.numeric(lin)
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_norm", {
  x <- N()
  set.seed(0)
  n_element <- 20
  n_draw <- 25
  ans_obtained <- generate(x, n_element = n_element, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  ans_expected <- matrix(rnorm(n_element * n_draw, sd = rep(sd, each = n_element)),
                         nrow = n_element)
  draw <- rep(seq_len(n_draw), each = n_element)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         element = rep(seq_len(n_element), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_normfixed", {
  x <- NFix(sd = 0.3)
  set.seed(0)
  n_element <- 20
  n_draw <- 25
  ans_obtained <- generate(x, n_element = n_element, n_draw = n_draw)
  set.seed(0)
  ans_expected <- matrix(rnorm(n_element * n_draw, sd = 0.3),
                         nrow = n_element)
  draw <- rep(seq_len(n_draw), each = n_element)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         element = rep(seq_len(n_element), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandom", {
  x <- RW()
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  ans_expected <- draw_vals_rw(sd = sd,
                               sd_init = 1,
                               matrix_along_by = matrix(seq_len(n_along) - 1L, nc = 1),
                               levels_effect = seq_len(n_along))
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandom, n_by = 2", {
  x <- RW()
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  value <- draw_vals_rw(sd = sd,
                        sd_init = 1,
                        matrix_along_by = matrix(seq_len(n_along * n_by) - 1L, nc = 2),
                        levels_effect = seq_len(n_along * n_by))
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandom, n_by = 2, con = 'by'", {
  x <- RW(con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  value <- draw_vals_rw(sd = sd,
                        sd_init = 1,
                        matrix_along_by = matrix(seq_len(n_along * n_by) - 1L, nc = 2),
                        levels_effect = seq_len(n_along * n_by))
  value <- as.numeric(value)
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandomseasfix", {
  x <- RW_Seas(n = 3, sd_seas = 0.2)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 1,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandomseasfix, n_by = 2", {
  x <- RW_Seas(n = 3, sd_seas = 0.2)
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 1,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- as.double(alpha + seas)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandomseasfix, n_by = 2, con = 'by'", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 1,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- as.double(alpha + seas)
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandomseasvary", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, s_seas = 0.3)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 1,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandomseasvary, n_by = 2", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, s_seas = 0.3)
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 1,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwrandomseasvary, n_by = 2, con = 'by'", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, s_seas = 0.3, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 1,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  value <- as.double(alpha + seas)
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzero", {
  x <- RW(sd = 0)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  ans_expected <- draw_vals_rw(sd = sd,
                               sd_init = 0,
                               matrix_along_by = matrix(seq_len(n_along) - 1L, nc = 1),
                               levels_effect = seq_len(n_along))
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzero, n_by = 2", {
  x <- RW(sd = 0)
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  ans_expected <- draw_vals_rw(sd = sd,
                               sd_init = 0,
                               matrix_along_by = matrix(seq_len(n_along * n_by) - 1L, nc = n_by),
                               levels_effect = seq_len(n_along * n_by))
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzero, n_by = 2, con = 'by'", {
  x <- RW(sd = 0, con = 'by')
  set.seed(0)
  n_along <- 20
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  value <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix(seq_len(n_along * n_by) - 1L, nc = n_by),
                        levels_effect = seq_len(n_along * n_by))
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzeroseasfix", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, sd = 0)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzeroseasfix, n_by = 2", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, sd = 0)
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzeroseasfix, n_by = 2, con = 'by'", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, sd = 0, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzeroseasvary", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, sd = 0, s_seas = 0.3)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzeroseasvary, n_by = 2", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, sd = 0, s_seas = 0.3)
  set.seed(0)
  n_along <- 20
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  value <- as.numeric(value)
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rwzeroseasvary, n_by = 2, con = 'by'", {
  x <- RW_Seas(n = 3, sd_seas = 0.2, sd = 0, s_seas = 0.3, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  alpha <- draw_vals_rw(sd = sd,
                        sd_init = 0,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2random", {
  x <- RW2()
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  ans_expected <- draw_vals_rw2(sd = sd,
                                sd_init = sd_init,
                                sd_slope = sd_slope,
                                matrix_along_by = matrix(seq_len(n_along) - 1L, nc = 1),
                                levels_effect = seq_len(n_along))
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2random, n_by = 2", {
  x <- RW2()
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  ans_expected <- draw_vals_rw2(sd = sd,
                                sd_init = sd_init,
                                sd_slope = sd_slope,
                                matrix_along_by = matrix(seq_len(n_along * n_by) - 1L, nc = n_by),
                                levels_effect = seq_len(n_along * n_by))
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2random, n_by = 2, con = 'by'", {
  x <- RW2(con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  value <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix(seq_len(n_along * n_by) - 1L, nc = n_by),
                         levels_effect = seq_len(n_along * n_by))
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2randomseasfix", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along) - 1L, nr = n_along)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2randomseasfix, n_by = 2", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2)
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2randomseasfix, n_by = 2, con = 'by'", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2randomseasvary", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, s_seas = 0.3)
  set.seed(0)
  n_along <- 10
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along) - 1L, nr = n_along)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2randomseasvary, n_by = 2", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, s_seas = 0.3)
  set.seed(0)
  n_along <- 20
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2randomseasvary, n_by = 2, con = 'by'", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, s_seas = 0.3, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  sd_init <- x$specific$sd
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = sd_init,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zero", {
  x <- RW2(sd = 0)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_slope <- x$specific$sd_slope
  ans_expected <- draw_vals_rw2(sd = sd,
                                sd_init = 0,
                                sd_slope = sd_slope,
                                matrix_along_by = matrix(seq_len(n_along) - 1L, nc = 1),
                                levels_effect = seq_len(n_along))
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zero, n_by = 2", {
  x <- RW2(sd = 0)
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_slope <- x$specific$sd_slope
  value <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix(seq_len(n_along * n_by) - 1L, nc = n_by),
                         levels_effect = seq_len(n_along * n_by))
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zero, n_by = 2, con = 'by'", {
  x <- RW2(sd = 0, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_slope <- x$specific$sd_slope
  value <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix(seq_len(n_along * n_by) - 1L, nc = n_by),
                         levels_effect = seq_len(n_along * n_by))
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zeroseasfix", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, sd = 0)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along) - 1L, nr = n_along)
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                        matrix_along_by = matrix_along_by,
                        levels_effect = seq_len(n_along))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zeroseasfix, n_by = 2", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, sd = 0)
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zeroseasfix, n_by = 2, con = 'by'", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, sd = 0, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasfix(n_seas = 3, sd_init = 0.2, matrix_along_by = matrix_along_by,
                            n_sim = n_draw)
  value <- alpha + seas
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zeroseasvary", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, sd = 0, s_seas = 0.3)
  set.seed(0)
  n_along <- 10
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along) - 1L, nr = n_along)
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  ans_expected <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(ans_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zeroseasvary, n_by = 2", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, sd = 0, s_seas = 0.3)
  set.seed(0)
  n_along <- 20
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_rw2zeroseasvary, n_by = 2, con = 'by'", {
  x <- RW2_Seas(n = 3, sd_seas = 0.2, sd = 0, s_seas = 0.3, con = "by")
  set.seed(0)
  n_along <- 10
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  sd <- draw_vals_sd(x, n_sim = n_draw)
  sd_seas <- draw_vals_sd_seas(x, n_sim = n_draw)
  matrix_along_by <- matrix(seq_len(n_along * n_by) - 1L, nr = n_along)
  sd_slope <- x$specific$sd_slope
  alpha <- draw_vals_rw2(sd = sd,
                         sd_init = 0,
                         sd_slope = sd_slope,
                         matrix_along_by = matrix_along_by,
                         levels_effect = seq_len(n_along * n_by))
  seas <- draw_vals_seasvary(n_seas = 3,
                             sd_innov = sd_seas,
                             sd_init = 0.2,
                             matrix_along_by = matrix_along_by)
  value <- alpha + seas
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_spline", {
  x <- Sp(n = 6)
  set.seed(0)
  n_along <- 20
  n_by <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  rw2 <- generate(RW2(), n_along = 6, n_draw = n_draw)$value
  rw2 <- matrix(rw2, nrow = 6)
  m <- make_matrix_spline(n_along = n_along, n_comp = 6)
  value <- m %*% rw2
  draw <- rep(seq_len(n_draw), each = n_along)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor("By 1"),
                         along = rep(seq_len(n_along), times = n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_spline, n_by = 2", {
  n_comp <- 6
  x <- Sp(n_comp = n_comp)
  set.seed(0)
  n_along <- 20
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  rw2 <- generate(RW2(), n_along = n_comp, n_by = n_by, n_draw = n_draw)$value
  rw2 <- matrix(rw2, nrow = n_comp)
  m <- make_matrix_spline(n_along = n_along, n_comp = n_comp)
  value <- m %*% rw2
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_spline, n_by = 2, con = 'by'", {
  n_comp <- 6
  x <- Sp(n_comp = n_comp, con = "by")
  set.seed(0)
  n_along <- 20
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  set.seed(0)
  rw2 <- generate(RW2(), n_along = n_comp, n_by = n_by, n_draw = n_draw)$value
  rw2 <- matrix(rw2, nrow = n_comp)
  m <- make_matrix_spline(n_along = n_along, n_comp = n_comp)
  value <- m %*% rw2
  value <- array(value, c(n_along, n_by, n_draw))
  means <- apply(value, c(1, 3), mean)
  value[,1,] <- value[,1,] - means
  value[,2,] <- value[,2,] - means
  draw <- rep(seq_len(n_draw), each = n_along * n_by)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- tibble(draw = draw,
                         by = factor(rep(rep(paste("By", 1:2), each = n_along), times = n_draw)),
                         along = rep(seq_len(n_along), times = n_by * n_draw),
                         value = as.double(value))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_svd - has sex", {
  x <- SVD(LFP)
  set.seed(0)
  n_element <- 1
  n_draw <- 25
  ans_obtained <- generate(x, n_element = n_element, n_draw = n_draw)
  set.seed(0)
  l <- generate_prior_svd_helper(x = x, n_element = n_element, n_draw = n_draw)
  alpha <- matrix(rnorm(ncol(l$matrix) * n_draw, sd = 1),
                  ncol = n_draw)
  value <- l$matrix %*% alpha + l$offset
  draw <- rep(seq_len(n_draw), each = n_element)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- l$ans
  ans_expected$value <- as.double(value)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'generate' works with bage_prior_svd - no sex", {
  x <- SVD(HFD)
  set.seed(0)
  n_element <- 2
  n_draw <- 25
  ans_obtained <- generate(x, n_element = n_element, n_draw = n_draw)
  set.seed(0)
  l <- generate_prior_svd_helper(x = x, n_element = n_element, n_draw = n_draw)
  alpha <- matrix(rnorm(ncol(l$matrix) * n_draw, sd = 1),
                  ncol = n_draw)
  value <- l$matrix %*% alpha + l$offset
  draw <- rep(seq_len(n_draw), each = n_element)
  draw <- paste("Draw", draw)
  draw <- factor(draw, levels = unique(draw))
  ans_expected <- l$ans
  ans_expected$value <- as.double(value)
  expect_equal(ans_obtained, ans_expected)
  !("sexgender" %in% names(ans_expected))
})

test_that("'generate' works with bage_prior_svd_ar - no sex, n_by = 2", {
  x <- SVD_AR(HFD)
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_ar - no sex, n_by = 2", {
  x <- SVD_AR(HFD, con = "by")
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rwrandom - no sex, n_by = 2", {
  x <- SVD_RW(HFD)
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rwrandom - has sex, n_by = 2, indep = TRUE", {
  x <- SVD_RW(LFP, indep = TRUE)
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "sexgender", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rwrandom - has sex, n_by = 2, indep = FALSE", {
  x <- SVD_RW(LFP, indep = FALSE)
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "sexgender", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rwrandom - has sex, n_by = 2, indep = FALSE, con = 'by'", {
  x <- SVD_RW(LFP, indep = FALSE, con = "by")
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "sexgender", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rwzero - no sex, n_by = 2", {
  x <- SVD_RW(HFD, sd = 0)
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rwzero - no sex, n_by = 2, con = 'by'", {
  x <- SVD_RW(HFD, sd = 0, con = "by")
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rw2random - no sex, n_by = 2", {
  x <- SVD_RW2(HFD)
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rw2random - no sex, n_by = 2, con = 'by'", {
  x <- SVD_RW2(HFD, con = "by")
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rw2zero - no sex, n_by = 2", {
  x <- SVD_RW2(LFP, sd = 0)
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "sexgender", "age", "value"))
})

test_that("'generate' works with bage_prior_svd_rw2zero - no sex, n_by = 2, con = 'by'", {
  x <- SVD_RW2(LFP, sd = 0, con = "by")
  set.seed(0)
  n_along <- 4
  n_by <- 2
  n_draw <- 5
  ans_obtained <- generate(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_setequal(names(ans_obtained), c("draw", "by", "along", "sexgender", "age", "value"))
})


## 'has_hyperrandfree' --------------------------------------------------------

test_that("'has_hyperrandfree' returns FALSE with prior without hyperrandfree", {
  prior <- N()
  expect_false(has_hyperrandfree(prior))
})

test_that("'has_hyperrandfree' returns TRUE with prior with hyperrandfree", {
  expect_true(has_hyperrandfree(Lin()))
  expect_true(has_hyperrandfree(Lin_AR()))
  expect_true(has_hyperrandfree(RW_Seas(n_seas=2)))
  expect_true(has_hyperrandfree(RW_Seas(n_seas=2, sd = 0)))
  expect_true(has_hyperrandfree(RW_Seas(n_seas=2, s_seas = 0.1)))
  expect_true(has_hyperrandfree(RW_Seas(n_seas=2, s_seas = 0.1, sd = 0)))
  expect_true(has_hyperrandfree(RW2_Seas(n_seas=2)))
  expect_true(has_hyperrandfree(RW2_Seas(n_seas=2, sd = 0)))
  expect_true(has_hyperrandfree(RW2_Seas(n_seas=2, s_seas = 0.1)))
  expect_true(has_hyperrandfree(RW2_Seas(n_seas=2, s_seas = 0.1, sd = 0)))
})


## 'infer_trend_cyc_seas_err_forecast_one' ------------------------------------

test_that("'infer_trend_cyc_seas_err_forecast_one' works with bage_prior_linar", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(sex:time ~ Lin_AR()) |>
    fit()
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  trend <- ans$.fitted[ans$component == "trend"]
  error <- ans$.fitted[ans$component == "error"]
  effect <- ans$.fitted[ans$component == "effect" & ans$term == "sex:time"]
  expect_equal(effect, trend + error)
})

test_that("'infer_trend_cyc_seas_err_forecast_one' works with bage_prior_rwrandomseasfix", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n_seas = 3)) |>
                  set_n_draw(n = 10) |>
                  fit()
  components <- components(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  expect_equal(effect, trend + season)
})

test_that("'infer_trend_cyc_seas_err_forecast_one' works with bage_prior_rwrandomseasvary", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2020, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n_seas = 3, s_seas = 0.5)) |>
                  fit()
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  expect_equal(effect, trend + season)
})

test_that("'infer_trend_cyc_seas_err_forecast_one' works with bage_prior_rwzeroseasfix", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n_seas = 3, sd = 0)) |>
                  set_n_draw(n = 10) |>
                  fit()
  components <- components(mod)
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  expect_equal(effect, trend + season)
})

test_that("'infer_trend_cyc_seas_err_forecast_one' works with bage_prior_rwzeroseasvary", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2020, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n_seas = 3, s_seas = 0.5, sd = 0)) |>
                  fit()
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  expect_equal(effect, trend + season)
})

test_that("'infer_trend_cyc_seas_err_one' works with bage_prior_rw2randomseasfix", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW2_Seas(n_seas = 3)) |>
                  set_n_draw(n = 10) |>
                  fit()
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  expect_equal(effect, trend + season)
})

test_that("'infer_trend_cyc_seas_err_one' works with bage_prior_rw2randomseasvary", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2020, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW2_Seas(n_seas = 3, s_seas = 1)) |>
                  set_n_draw(n_draw = 10) |>
                  fit()
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  expect_equal(effect, trend + season)
})

test_that("'infer_trend_cyc_seas_err_one' works with bage_prior_rw2randomseasfix", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW2_Seas(n_seas = 3, sd = 0)) |>
                  set_n_draw(n = 10) |>
                  fit()
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  expect_equal(effect, trend + season)
})

test_that("'infer_trend_cyc_seas_err_one' works with bage_prior_rw2randomseasvary", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2020, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW2_Seas(n_seas = 3, s_seas = 1, sd = 0)) |>
                  set_n_draw(n_draw = 10) |>
                  fit()
  term <- make_term_components(mod)
  comp <- make_comp_components(mod)
  level <- make_level_components(mod)
  draws <- make_draws_components(mod)
  draws <- as.matrix(draws)
  .fitted <- rvec::rvec_dbl(draws)
  components <- tibble::tibble(term = term,
                               component = comp,
                               level = level,
                               .fitted = .fitted)
  ans <- infer_trend_cyc_seas_err_forecast_one(prior = mod$priors[["sex:time"]],
                                               dimnames_term = mod$dimnames_terms[["sex:time"]],
                                               var_time = mod$var_time,
                                               var_age = mod$var_age,
                                               components = components)
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  expect_equal(effect, trend + season)
})


## is_known -------------------------------------------------------------------

test_that("'is_known' works with valid inputs", {
    expect_false(is_known(N()))
    expect_true(is_known(Known(values = 1:3)))
})


## is_prior_ok_for_term -------------------------------------------------------

test_that("'is_prior_ok_for_term' works with bage_prior_ar1 - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = AR1(),
                                     dimnames_term = list(time = 2001:2004),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_ar - n_by = 3", {
    expect_true(is_prior_ok_for_term(prior = AR(n_coef = 3),
                                     dimnames_term = list(time = 2001:2010,
                                                          reg = 1:3),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' throws expected error with bage_prior_known", {
  expect_error(is_prior_ok_for_term(prior = Known(c(0.1, -0.1)),
                                    dimnames_term = list(sex = c("f", "m", "d")),
                                    var_time = "time",
                                    var_age = "age",
                                    var_sexgender = "sex"),
               "`Known\\(c\\(0.1,-0.1\\)\\)` prior for `sex` term invalid.")    
})

test_that("'is_prior_ok_for_term' works with bage_prior_lin - n_by = 2", {
    expect_true(is_prior_ok_for_term(prior = Lin(),
                                     dimnames_term = list(sex = c("f", "m"),
                                                          time = 1:6),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_lin - n_by = 1", {
  expect_true(is_prior_ok_for_term(prior = Lin(),
                                   dimnames_term = list(time = 1:2),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_linar - n_by = 1", {
  expect_true(is_prior_ok_for_term(prior = Lin_AR(),
                                   dimnames_term = list(time = 1:3),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_linex - n_by = 1", {
  expect_true(is_prior_ok_for_term(prior = Lin(s = 0),
                                   dimnames_term = list(time = 1:2),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_norm", {
    expect_true(is_prior_ok_for_term(prior = N(),
                                     dimnames_term = list(sex = c("f", "m")),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_normfixed", {
    expect_true(is_prior_ok_for_term(prior = NFix(),
                                     dimnames_term = list(sex = c("f", "m")),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rwrandom - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = RW(),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_rwrandom - n_by = 3", {
  expect_true(is_prior_ok_for_term(prior = RW(),
                                   dimnames_term = list(age = 1:3,
                                                        time = 2001:2010),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rwrandomseasfix", {
    expect_true(is_prior_ok_for_term(prior = RW_Seas(n_seas = 2),
                                     dimnames_term = list(time = 2001:2003),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rwrandomseasvary", {
    expect_true(is_prior_ok_for_term(prior = RW_Seas(n_seas = 2, s_seas = 1),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rwzero - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = RW(sd = 0),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_rwzero - n_by = 3", {
  expect_true(is_prior_ok_for_term(prior = RW(sd = 0),
                                   dimnames_term = list(age = 1:3,
                                                        time = 2001:2010),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rwzeroseasfix", {
    expect_true(is_prior_ok_for_term(prior = RW_Seas(n_seas = 2, sd = 0),
                                     dimnames_term = list(time = 2001:2003),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rwzeroseasvary", {
    expect_true(is_prior_ok_for_term(prior = RW_Seas(n_seas = 2, sd = 0, s_seas = 1),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2infant", {
    expect_true(is_prior_ok_for_term(prior = RW2_Infant(),
                                     dimnames_term = list(age = 0:4),
                                     var_time = "time",
                                     var_age = "age"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_rw2random - n_by = 3", {
  expect_true(is_prior_ok_for_term(prior = RW2(),
                                   dimnames_term = list(age = 1:3, time = 2001:2010),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2random - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = RW2(),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2randomseasfix", {
    expect_true(is_prior_ok_for_term(prior = RW2_Seas(n_seas = 2),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2randomseasvary", {
    expect_true(is_prior_ok_for_term(prior = RW2_Seas(n_seas = 2, s_seas = 1),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' throws correct error with bage_prior_rw2zero - n_by = 3", {
  expect_true(is_prior_ok_for_term(prior = RW2(sd = 0),
                                   dimnames_term = list(age = 1:3, time = 2001:2010),
                                   var_time = "time",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2zero - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = RW2(sd = 0),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2zeroseasfix", {
    expect_true(is_prior_ok_for_term(prior = RW2_Seas(n_seas = 2, sd = 0),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_rw2zeroseasvary", {
    expect_true(is_prior_ok_for_term(prior = RW2_Seas(n_seas = 2, sd = 0, s_seas = 1),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_spline - n_by = 1", {
    expect_true(is_prior_ok_for_term(prior = Sp(),
                                     dimnames_term = list(time = 2001:2010),
                                     var_time = "time",
                                     var_age = "age",
                                     var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd, correct inputs", {
  expect_true(is_prior_ok_for_term(prior = SVD(HMD),
                                   dimnames_term = list(age = c(0:59, "60+"),
                                                        year = 1:5),
                                   var_time = "year",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd_ar, correct inputs", {
  expect_true(is_prior_ok_for_term(prior = SVD_AR(HMD),
                                   dimnames_term = list(age = c(0:59, "60+"),
                                                        sex = 1:2,
                                                        year = 1:5),
                                   var_time = "year",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd_rwrandom, correct inputs", {
  expect_true(is_prior_ok_for_term(prior = SVD_RW(HMD),
                                   dimnames_term = list(age = c(0:59, "60+"),
                                                        sex = 1:2,
                                                        year = 1:5),
                                   var_time = "year",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd_rwzero, correct inputs", {
  expect_true(is_prior_ok_for_term(prior = SVD_RW(HMD, sd = 0),
                                   dimnames_term = list(age = c(0:59, "60+"),
                                                        sex = 1:2,
                                                        year = 1:5),
                                   var_time = "year",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd_rw2random, correct inputs", {
  expect_true(is_prior_ok_for_term(prior = SVD_RW2(HMD),
                                   dimnames_term = list(age = c(0:59, "60+"),
                                                        sex = 1:2,
                                                        year = 1:5),
                                   var_time = "year",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})

test_that("'is_prior_ok_for_term' works with bage_prior_svd_rw2zero, correct inputs", {
  expect_true(is_prior_ok_for_term(prior = SVD_RW2(HMD, sd = 0),
                                   dimnames_term = list(age = c(0:59, "60+"),
                                                        sex = 1:2,
                                                        year = 1:5),
                                   var_time = "year",
                                   var_age = "age",
                                   var_sexgender = "sex"))
})


## is_spline ------------------------------------------------------------------

test_that("'is_spline' works with valid inputs", {
    expect_false(is_spline(N()))
    expect_true(is_spline(Sp()))
})


## is_svd ---------------------------------------------------------------------

test_that("'is_svd' works with valid inputs", {
    expect_false(is_svd(N()))
    expect_true(is_svd(SVD(HMD)))
    expect_true(is_svd(SVD_AR(HMD)))
    expect_true(is_svd(SVD_RW(HMD)))
    expect_true(is_svd(SVD_RW(HMD, sd = 0)))
    expect_true(is_svd(SVD_RW2(HMD)))
    expect_true(is_svd(SVD_RW2(HMD, sd = 0)))
})


## length_hyperrandfree -------------------------------------------------------

test_that("'length_hyperrandfree' works with 'bage_prior_ar'", {
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = AR(n_coef = 2),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 0L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'length_hyperrandfree' works with 'bage_prior_lin', con is 'none'", {
  dimnames_term <- list(x = letters[1:13], y = c("a", "b"))
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = Lin(along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_lin', con is 'by'", {
  dimnames_term <- list(x = letters[1:13], y = c("a", "b"))
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = Lin(along = "x", con = 'by'),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_lin' - n_by = 1", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = Lin(along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_linar', con is 'none'", {
  dimnames_term <- list(x = letters[1:13],
                        y = c("a", "b"))
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = Lin_AR(along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_linar', con is 'by'", {
  dimnames_term <- list(x = letters[1:13],
                        y = c("a", "b"))
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = Lin_AR(along = "x", con = 'by'),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_linar' - n_by = 1", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = Lin_AR(along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwrandomseasfix' - main effect", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwrandomseasfix' - main effect", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 2, along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwrandomseasfix' - interaction, con is 'none'", {
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 4L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwrandomseasfix' - interaction, con is 'by'", {
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, con = 'by'),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwrandomseasvary' - interaction, con is 'none'", {
  dimnames_term <- list(time = 2001:2013,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, s_seas = 1),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 18L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwrandomseasvary' - interaction, con is 'by'", {
  dimnames_term <- list(time = 2001:2013,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, s_seas = 1, con = 'by'),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 9L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwzeroseasfix' - main effect", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, along = "x", sd = 0),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwzeroseasfix' - main effect", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 2, sd = 0, along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 0L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwzeroseasfix' - interaction, con is 'none'", {
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, sd = 0),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwzeroseasfix' - interaction, con is 'by'", {
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, sd = 0, con = 'by'),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwzeroseasvary' - interaction, con is 'none'", {
  dimnames_term <- list(time = 2001:2013,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, sd = 0, s_seas = 1),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 16L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rwzeroseasvary' - interaction, con is 'by'", {
  dimnames_term <- list(time = 2001:2013,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW_Seas(n_seas = 3, sd = 0, s_seas = 1, con = 'by'),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 8L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rw2randomseasfix'", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW2_Seas(n_seas = 3, s_seas = 0, along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rw2randomseasvary'", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW2_Seas(n_seas = 3, s_seas = 1, along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 9L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rw2zeroseasfix'", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW2_Seas(n_seas = 3, sd = 0, along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'length_hyperrandfree' works with 'bage_prior_rw2zeroseasvary'", {
  dimnames_term <- list(x = letters[1:13])
  var_time <- "time"
  var_age <- "age"
  var_varsexgender <- "sex"
  ans_obtained <- length_hyperrandfree(prior = RW2_Seas(n_seas = 3, sd = 0, s_seas = 1, along = "x"),
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- 8L
  expect_identical(ans_obtained, ans_expected)                   
})


## levels_hyper ---------------------------------------------------------------

test_that("'levels_hyper' works with 'bage_prior_ar'", {
  expect_identical(levels_hyper(prior = AR(n_coef = 2)),
                   c("coef1", "coef2", "sd"))
  expect_identical(levels_hyper(prior = AR1()),
                   c("coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_known'", {
  expect_identical(levels_hyper(prior = Known(1)),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_lin'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Lin()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_linar'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = Lin_AR()),
                   c("coef1", "coef2", "sd"))
  expect_identical(levels_hyper(prior = Lin_AR1()),
                   c("coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_linex'", {
  expect_identical(levels_hyper(prior = Lin(s = 0)),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_norm'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = N()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_normfixed'", {
  matrix_along_by <- matrix(0:9, ncol = 1L)
  expect_identical(levels_hyper(prior = NFix()),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_rwrandom'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rwrandomseasfix'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW_Seas(n_seas = 3)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rwrandomseasvary'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW_Seas(n_seas = 3, s_seas = 1)),
                   c("sd_seas", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_rwzero'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW(sd = 0)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rwzeroseasfix'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW_Seas(n_seas = 3, sd = 0)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rwzeroseasvary'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW_Seas(n_seas = 3, s_seas = 1, sd = 0)),
                   c("sd_seas", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_rw2infant'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2_Infant()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2random'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2()),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2randomseasfix'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2_Seas(n_seas = 3)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2randomseasvary'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2_Seas(n_seas = 3, s_seas = 1)),
                   c("sd_seas", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_rw2zero'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2(sd = 0)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2zeroseasfix'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2_Seas(n_seas = 3, sd = 0)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_rw2zeroseasvary'", {
  matrix_along_by <- matrix(0:9, ncol = 2L)
  expect_identical(levels_hyper(prior = RW2_Seas(n_seas = 3, s_seas = 1, sd = 0)),
                   c("sd_seas", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_spline'", {
  expect_identical(levels_hyper(prior = Sp()), 
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_svd'", {
  expect_identical(levels_hyper(prior = SVD(sim_ssvd())),
                   character())
})

test_that("'levels_hyper' works with 'bage_prior_svd_ar'", {
  expect_identical(levels_hyper(prior = SVD_AR(HMD)),
                   c("coef1", "coef2", "sd"))
  expect_identical(levels_hyper(prior = SVD_AR1(HMD)),
                   c("coef", "sd"))
})

test_that("'levels_hyper' works with 'bage_prior_svd_rwrandom'", {
  expect_identical(levels_hyper(prior = SVD_RW(HMD)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_svd_rwzero'", {
  expect_identical(levels_hyper(prior = SVD_RW(HMD, sd = 0)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_svd_rw2random'", {
  expect_identical(levels_hyper(prior = SVD_RW2(HMD)),
                   "sd")
})

test_that("'levels_hyper' works with 'bage_prior_svd_rw2zero'", {
  expect_identical(levels_hyper(prior = SVD_RW2(HMD, sd = 0)),
                   "sd")
})


## levels_hyperrand ---------------------------------------------------------------

test_that("'levels_hyperrand' works with 'bage_prior_ar'", {
  dimnames_term <- list(time = 2001:2010)
  ans_obtained <- levels_hyperrand(prior = AR(n_coef = 2),
                                   dimnames_term = dimnames_term)
  ans_expected <- character()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'levels_hyperrand' works with 'bage_prior_lin'", {
  dimnames_term <- list(x = letters[1:13], y = c("a", "b"))
  ans_obtained <- levels_hyperrand(prior = Lin(along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- c("slope.a", "slope.b", rep(dimnames_to_levels(dimnames_term), 2))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_lin' - n_by = 1", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- levels_hyperrand(prior = Lin(along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- c("slope", rep(dimnames_to_levels(dimnames_term), 2))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_linar'", {
  dimnames_term <- list(x = letters[1:13],
                        y = c("a", "b"))
  ans_obtained <- levels_hyperrand(prior = Lin_AR(along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- c("slope.a", "slope.b", rep(dimnames_to_levels(dimnames_term), 2))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_linar' - n_by = 1", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- levels_hyperrand(prior = Lin_AR(along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- c("slope", rep(dimnames_to_levels(dimnames_term), 2))
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rwrandomseasfix' - main effect", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- levels_hyperrand(prior = RW_Seas(n_seas = 3, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rwrandomseasfix' - interaction", {
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2010)
  ans_obtained <- levels_hyperrand(prior = RW_Seas(n_seas = 3),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rwrandomseasvary' - interaction", {
  dimnames_term <- list(time = 2001:2013,
                        sex = c("f", "m"))
  ans_obtained <- levels_hyperrand(prior = RW_Seas(n_seas = 3, s_seas = 1),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rwzeroseasfix' - main effect", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- levels_hyperrand(prior = RW_Seas(n_seas = 3, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rwzeroseasfix' - interaction", {
  dimnames_term <- list(sex = c("f", "m"),
                        time = 2001:2010)
  ans_obtained <- levels_hyperrand(prior = RW_Seas(n_seas = 3),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rwzeroseasvary' - interaction", {
  dimnames_term <- list(time = 2001:2013,
                        sex = c("f", "m"))
  ans_obtained <- levels_hyperrand(prior = RW_Seas(n_seas = 3, s_seas = 1),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rw2randomseasfix'", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- levels_hyperrand(prior = RW2_Seas(n_seas = 3, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rw2randomseasvary'", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- levels_hyperrand(prior = RW2_Seas(n_seas = 3, s_seas = 1, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rw2zeroseasfix'", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- levels_hyperrand(prior = RW2_Seas(n_seas = 3, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})

test_that("'levels_hyperrand' works with 'bage_prior_rw2zeroseasvary'", {
  dimnames_term <- list(x = letters[1:13])
  ans_obtained <- levels_hyperrand(prior = RW2_Seas(n_seas = 3, s_seas = 1, along = "x"),
                                   dimnames_term = dimnames_term)
  ans_expected <- rep(dimnames_to_levels(dimnames_term), 2)
  expect_identical(ans_obtained, ans_expected)                   
})


## 'make_hyperrand_one' ---------------------------------------------

test_that("'make_hyperrand_one' works with prior with no hyperrand", {
  set.seed(0)
  prior <- AR()
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                                     hyperrandfree = hyperrandfree,
                                     effectfree = effectfree,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  expect_identical(ans, NULL)
})

test_that("'make_hyperrand_one' works with bage_prior_lin", {
  set.seed(0)
  prior <- Lin()
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                                     hyperrandfree = hyperrandfree,
                                     effectfree = effectfree,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  expect_identical(length(ans), 21L)
})

test_that("'make_hyperrand_one' works with bage_prior_linar", {
  set.seed(0)
  prior <- Lin_AR()
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 21L)
})

test_that("'make_hyperrand_one' works with bage_prior_rwrandomseasfix", {
  set.seed(0)
  prior <- RW_Seas(n = 3)
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 20L)
})

test_that("'make_hyperrand_one' works with bage_prior_rwrandomseasvary", {
  set.seed(0)
  prior <- RW_Seas(n = 3, s_seas = 0.2)
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 20L)
})

test_that("'make_hyperrand_one' works with bage_prior_rwzeroseasfix", {
  set.seed(0)
  prior <- RW_Seas(n = 3, sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 20L)
})

test_that("'make_hyperrand_one' works with bage_prior_rwzeroseasvary", {
  set.seed(0)
  prior <- RW_Seas(n = 3, s_seas = 0.2, sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 20L)
})

test_that("'make_hyperrand_one' works with bage_prior_rw2randomseasfix", {
  set.seed(0)
  prior <- RW2_Seas(n = 3)
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 20L)
})

test_that("'make_hyperrand_one' works with bage_prior_rw2randomseasvary", {
  set.seed(0)
  prior <- RW2_Seas(n = 3)
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 20L)
})

test_that("'make_hyperrand_one' works with bage_prior_rw2zeroseasfix", {
  set.seed(0)
  prior <- RW2_Seas(n = 3, sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 20L)
})

test_that("'make_hyperrand_one' works with bage_prior_rw2zeroseasvary", {
  set.seed(0)
  prior <- RW2_Seas(n = 3, sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_hyperrand_one(prior = prior,
                            hyperrandfree = hyperrandfree,
                            effectfree = effectfree,
                            dimnames_term = dimnames_term,
                            var_time = var_time,
                            var_age = var_age,
                            var_sexgender = var_sexgender)
  expect_identical(length(ans), 20L)
})



## 'make_i_along' -------------------------------------------------------------

test_that("'make_i_along' works with bage_prior_ar", {
  prior <- AR()
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_lin", {
  prior <- Lin()
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_linar", {
  prior <- Lin_AR1()
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_linex", {
  prior <- Lin(s = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rwrandom", {
  prior <- RW()
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rwrandomseasfix", {
  prior <- RW_Seas(n_seas = 2)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rwrandomseasvary", {
  prior <- RW_Seas(n_seas = 2, s_seas = 1)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rwzero", {
  prior <- RW(sd = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rwzeroseasfix", {
  prior <- RW_Seas(n_seas = 2, sd = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rwzeroseasvary", {
  prior <- RW_Seas(n_seas = 2, s_seas = 1, sd = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rw2infant", {
  prior <- RW2_Infant()
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 1L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rw2random", {
  prior <- RW2()
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rw2randomseasfix", {
  prior <- RW2_Seas(n = 2)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rw2randomseasvary", {
  prior <- RW2_Seas(n = 2, s = 1)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rw2zero", {
  prior <- RW2(sd = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rw2zeroseasfix", {
  prior <- RW2_Seas(n = 2, sd = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_rw2zeroseasvary", {
  prior <- RW2_Seas(n = 2, s = 1, sd = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_sp", {
  prior <- Sp()
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_svd_ar", {
  prior <- SVD_AR(HMD)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_svd_rwrandom", {
  prior <- SVD_RW(HMD)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_svd_rwzero", {
  prior <- SVD_RW(HMD, sd = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_svd_rw2random", {
  prior <- SVD_RW2(HMD)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_i_along' works with bage_prior_svd_rw2zero", {
  prior <- SVD_RW2(HMD, sd = 0)
  dimnames_term <- list(age = 1:3,
                        time = 2000:2005,
                        sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- make_i_along(prior = prior,
                               dimnames_term = dimnames_term,
                               var_time = var_time,
                               var_age = var_age)
  ans_expected <- 2L
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by_effectfree' ------------------------------------------------

test_that("default for 'make_matrix_along_by_effectfree' works - intercept", {
  prior <- NFix()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0L, nrow = 1L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("default for 'make_matrix_along_by_effectfree' works", {
  prior <- N()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(reg = 1:3),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:2, nrow = 3L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with bage_prior_ar", {
  prior <- AR1()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with bage_prior_ar = con is 'by'", {
  prior <- AR1(con = 'by')
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010,
                                                                       sex = c("f", "m")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_lin", {
  prior <- Lin()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 0:4),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:4, nr = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_linar", {
  prior <- Lin_AR1()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 0:4),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:4, nr = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_linex", {
  prior <- Lin(s = 0)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 0:4),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:4, nr = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rwrandom", {
  prior <- RW()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = 0:4),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:4, nr = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_rwrandom', 2 dimensions, con is 'none'", {
  prior <- RW()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 0:9, region = c("a", "b")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:19,
                         nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rwrandomseasfix", {
  prior <- RW_Seas(n_seas = 2)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rwrandomseasvary", {
  prior <- RW_Seas(n_seas = 3, s_seas = 1)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_rwzero', 2 dimensions, con is 'none'", {
  prior <- RW(sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 0:9, region = c("a", "b")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:17,
                         nr = 9)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rwzeroseasfix", {
  prior <- RW_Seas(n_seas = 2, sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rwzeroseasvary", {
  prior <- RW_Seas(n_seas = 3, s_seas = 1, sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_rw2infant', 2 dimensions, con is 'none'", {
  prior <- RW2_Infant()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = 0:9, region = c("a", "b")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:19,
                         nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_rw2infant', 2 dimensions, con is 'by'", {
  prior <- RW2_Infant(con = 'by')
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = 0:9, region = c("a", "b")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9,
                         nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rw2infant", {
  prior <- RW2_Infant()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = 0:9),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rw2random, 1 dimension", {
  prior <- RW2()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = 0:4),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:4, nr = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_rw2random', 2 dimensions", {
  prior <- RW2()
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 0:9, region = c("a", "b")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:19,
                         nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rw2randomseasfix", {
  prior <- RW2_Seas(n_seas = 5)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rw2randomseasvary", {
  prior <- RW2_Seas(n_seas = 4, s_seas = 1)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rw2zero, 1 dimension", {
  prior <- RW2(sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = 0:4),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:3, nr = 4)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_rw2zero', 2 dimensions", {
  prior <- RW2(sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 0:9, region = c("a", "b")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:17,
                         nr = 9)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rw2zeroseasfix", {
  prior <- RW2_Seas(n_seas = 5)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works - bage_prior_rw2zeroseasvary", {
  prior <- RW2_Seas(n_seas = 4, s_seas = 1)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(time = 2001:2010),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:9, nr = 10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_spline'", {
  prior <- Sp(n_comp = 6)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = 0:9, region = c("a", "b")),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:11,
                         nr = 6)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_svd' - total", {
  prior <- SVD(HMD)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = c(0:79, "80+"),
                                                                       region = c("a", "b")),
                                                  var_time = NULL,
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- matrix(0:5, nr = 3)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_svd_ar' - indep", {
  prior <- SVD_AR(HMD)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = c(0:79, "80+"),
                                                                  sex = c("f", "m"),
                                                                  time = 2001:2005),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = "sex")
  ans_expected <- t(matrix(0:29, nr = 6))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_svd_ar'", {
  prior <- SVD_AR1(HMD)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = c(0:79, "80+"),
                                                                  time = 2001:2005),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- t(matrix(0:14, nr = 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_svd_rwrandom'", {
  prior <- SVD_RW(HMD)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = c(0:79, "80+"),
                                                                  time = 2001:2005),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- t(matrix(0:14, nr = 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_svd_rwzero'", {
  prior <- SVD_RW(HMD, sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = c(0:79, "80+"),
                                                                  time = 2001:2005),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- t(matrix(0:11, nr = 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_svd_rw2random'", {
  prior <- SVD_RW2(HMD)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = c(0:79, "80+"),
                                                                  time = 2001:2005),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- t(matrix(0:14, nr = 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree' works with 'bage_prior_svd_rw2zero'", {
  prior <- SVD_RW2(HMD, sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree(prior = prior,
                                                  dimnames_term = list(age = c(0:79, "80+"),
                                                                  time = 2001:2005),
                                                  var_time = "time",
                                                  var_age = "age",
                                                  var_sexgender = NULL)
  ans_expected <- t(matrix(0:11, nr = 3))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by_effectfree_innermost' --------------------------------

test_that("default for 'make_matrix_along_by_effectfree_innermost' works", {
  prior <- RW()
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(time = 1:10),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = NULL,
                                                            dim = 9L)
  ans_expected <- make_matrix_along_by_inner(1L, 9L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_innermost' method for bage_prior_spline works", {
  prior <- Sp(n_comp = 5)
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(sex = 1:2,
                                                                                 age = 1:10),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = "sex",
                                                            dim = c(2L, 10L))
  ans_expected <- make_matrix_along_by_inner(2L, c(2L, 5L))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_innermost' method for bage_prior_svd works - age, sex", {
  prior <- SVD(HMD, n_comp = 4)
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(sex = c("f", "m"),
                                                                                 age = c(0:99,
                                                                                         "100+")),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = "sex",
                                                            dim = c(2L, 101L))
  ans_expected <- make_matrix_along_by_inner(1L, 8L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_innermost' method for bage_prior_svd works, - age, sex, reg", {
  prior <- SVD(HMD, n_comp = 4)
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(sex = c("f", "m"),
                                                                                 age = c(0:99,
                                                                                         "100+"),
                                                                                 region = 1:3),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = "sex",
                                                            dim = c(2L, 101L, 3L))
  ans_expected <- make_matrix_along_by_inner(1L, c(8L, 3L))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_innermost' method for bage_prior_svd_ar works, - age, sex, time", {
  prior <- SVD_AR(HMD, n_comp = 4)
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(sex = c("f", "m"),
                                                                                 age = c(0:99,
                                                                                         "100+"),
                                                                                 time = 1:10),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = "sex",
                                                            dim = c(2L, 101L, 10L))
  ans_expected <- make_matrix_along_by_inner(2L, c(8L, 10L))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_innermost' method for bage_prior_svd_rwrandom works, - age, sex, time", {
  prior <- SVD_RW(HMD, n_comp = 4)
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(sex = c("f", "m"),
                                                                                 age = c(0:99,
                                                                                         "100+"),
                                                                                 time = 1:10),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = "sex",
                                                            dim = c(2L, 101L, 10L))
  ans_expected <- make_matrix_along_by_inner(2L, c(8L, 10L))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_innermost' method for bage_prior_svd_rwzero works, - age, sex, time", {
  prior <- SVD_RW(HMD, n_comp = 4, sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(sex = c("f", "m"),
                                                                                 age = c(0:99,
                                                                                         "100+"),
                                                                                 time = 1:10),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = "sex",
                                                            dim = c(2L, 101L, 10L))
  ans_expected <- make_matrix_along_by_inner(2L, c(8L, 10L))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_innermost' method for bage_prior_svd_rw2random works, - age, sex, time", {
  prior <- SVD_RW2(HMD, n_comp = 4)
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(sex = c("f", "m"),
                                                                                 age = c(0:99,
                                                                                         "100+"),
                                                                                 time = 1:10),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = "sex",
                                                            dim = c(2L, 101L, 10L))
  ans_expected <- make_matrix_along_by_inner(2L, c(8L, 10L))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by_effectfree_innermost' method for bage_prior_svd_rw2zero works, - age, sex, time", {
  prior <- SVD_RW2(HMD, n_comp = 4, sd = 0)
  ans_obtained <- make_matrix_along_by_effectfree_innermost(prior = prior,
                                                            dimnames_term = list(sex = c("f", "m"),
                                                                                 age = c(0:99,
                                                                                         "100+"),
                                                                                 time = 1:10),
                                                            var_time = "time",
                                                            var_age = "age",
                                                            var_sexgender = "sex",
                                                            dim = c(2L, 101L, 10L))
  ans_expected <- make_matrix_along_by_inner(2L, c(8L, 10L))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_draws_svd' ----------------------------------------------------

test_that("'make_matrix_draws_svd' works with bage_prior_norm", {
  set.seed(0)
  prior <- N()
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_draws_svd(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_draws_svd' works with bage_prior_svd", {
  set.seed(0)
  prior <- SVD(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_draws_svd(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender)
  ans_expected <- Matrix::.sparseDiagonal(30)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_draws_svd' works with bage_prior_svd_ar", {
  set.seed(0)
  prior <- SVD_AR(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_draws_svd(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender)
  ans_expected <- Matrix::.sparseDiagonal(30)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_draws_svd' works with bage_prior_svd_rwrandom", {
  set.seed(0)
  prior <- SVD_RW(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_draws_svd(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender)
  ans_expected <- Matrix::.sparseDiagonal(30)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_draws_svd' works with bage_prior_svd_rwzero", {
  set.seed(0)
  prior <- SVD_RW(HMD, sd = 0)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  m <- make_matrix_draws_svd(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                             var_sexgender = var_sexgender)
  x <- matrix(rnorm(3 * 9), nr = 3)
  ans_obtained <- as.numeric(m %*% as.numeric(x))
  ans_expected <- as.numeric(cbind(0, x))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_draws_svd' works with bage_prior_svd_rw2random", {
  set.seed(0)
  prior <- SVD_RW2(HMD)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_draws_svd(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                                        var_sexgender = var_sexgender)
  ans_expected <- Matrix::.sparseDiagonal(30)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_draws_svd' works with bage_prior_svd_rw2zero", {
  set.seed(0)
  prior <- SVD_RW2(HMD, sd = 0)
  dimnames_term <- list(age = poputils::age_labels(type = "lt", max = 60),
                        time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  m <- make_matrix_draws_svd(prior = prior,
                                        dimnames_term = dimnames_term,
                                        var_time = var_time,
                                        var_age = var_age,
                             var_sexgender = var_sexgender)
  x <- matrix(rnorm(3 * 9), nr = 3)
  ans_obtained <- as.numeric(m %*% as.numeric(x))
  ans_expected <- as.numeric(cbind(0, x))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_effectfree_effect' --------------------------------------------------

test_that("'make_matrix_effectfree_effect' works with bage_prior_ar1 - con is 'none'", {
  prior <- AR1()
  dimnames_term <- list(time = 2001:2005)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- diag(5)
  expect_identical(as.matrix(ans_obtained), ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_ar1 - interaction, con is 'by'", {
  prior <- AR1(con = 'by')
  dimnames_term <- list(time = 2001:2005, reg = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(5, 2))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_lin - interaction, con is 'by'", {
  prior <- Lin(con = 'by')
  dimnames_term <- list(time = 2001:2005, reg = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(5, 2))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_linar - interaction, con is 'by'", {
  prior <- Lin_AR(con = 'by')
  dimnames_term <- list(time = 2001:2005, reg = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(5, 2))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_linex - interaction, along first, con is 'none'", {
  prior <- Lin(s = 0)
  dimnames_term <- list(time = 2001:2005, reg = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- Matrix::kronecker(Matrix::.sparseDiagonal(2),
                                    matrix(1:5 - 3, nc = 1))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_linex - interaction, along second, con is 'none'", {
  prior <- Lin(s = 0)
  dimnames_term <- list(reg = 1:2, time = 2001:2005)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- Matrix::kronecker(matrix(1:5 - 3, nr = 5),
                                    Matrix::.sparseDiagonal(2))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_linex - interaction, along first, con is 'by'", {
  prior <- Lin(s = 0, con = 'by')
  dimnames_term <- list(time = 2001:2005, reg = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  expect_equal(as.matrix(ans_obtained)[1:5], -as.matrix(ans_obtained)[6:10])
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_linex - interaction, along second, con is 'by'", {
  prior <- Lin(s = 0, con = 'by')
  dimnames_term <- list(reg = 1:3, time = 2001:2005)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  m <- make_matrix_effectfree_effect(prior = prior,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  x <- rnorm(2)
  v <- matrix(as.numeric(m %*% x), nr = 3)
  expect_equal(colSums(v), rep(0, 5))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwrandom - main effect", {
  prior <- RW()
  dimnames_term <- list(age = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- Matrix::.sparseDiagonal(10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwrandom - interaction, con is 'none'", {
  prior <- RW()
  dimnames_term <- list(age = 1:10, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- Matrix::kronecker(diag(3), Matrix::.sparseDiagonal(10))
  expect_identical(Matrix::as.matrix(ans_obtained), Matrix::as.matrix(ans_expected))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwrandom - interaction, con is 'by'", {
  prior <- RW(con = 'by')
  dimnames_term <- list(age = 1:10, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- Matrix::kronecker(make_matrix_unconstr_constr(3),
                                    Matrix::.sparseDiagonal(10))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwrandomseasfix - interaction, con is 'by'", {
  prior <- RW_Seas(n_seas = 2, con = 'by')
  dimnames_term <- list(time = 2001:2010, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwrandomseasvary - interaction, con is 'by'", {
  prior <- RW_Seas(n_seas = 2, con = 'by', s_seas = 0.2)
  dimnames_term <- list(time = 2001:2010, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwzero - main effect", {
  prior <- RW(sd = 0)
  dimnames_term <- list(age = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- rbind(0, Matrix::.sparseDiagonal(9))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwzero - interaction, con is 'none'", {
  prior <- RW(sd = 0)
  dimnames_term <- list(age = 1:10, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- Matrix::kronecker(Matrix::.sparseDiagonal(3),
                                    rbind(0, Matrix::.sparseDiagonal(9)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwzero - interaction, con is 'by'", {
  prior <- RW(con = 'by', sd = 0)
  dimnames_term <- list(age = 1:10, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- Matrix::kronecker(make_matrix_unconstr_constr(3),
                                    rbind(0, Matrix::.sparseDiagonal(9)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwzeroseasfix - interaction, con is 'by'", {
  prior <- RW_Seas(n_seas = 2, con = 'by', sd = 0)
  dimnames_term <- list(time = 2001:2010, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rwzeroseasvary - interaction, con is 'by'", {
  prior <- RW_Seas(n_seas = 2, con = 'by', s_seas = 0.2, sd = 0)
  dimnames_term <- list(time = 2001:2010, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rw2infant - interaction, con is 'by'", {
  prior <- RW2_Infant(con = 'by')
  dimnames_term <- list(age = 0:9, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rw2random", {
  prior <- RW2()
  dimnames_term <- list(age = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- Matrix::.sparseDiagonal(10)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rw2randomseasfix - interaction, con is 'by'", {
  prior <- RW2_Seas(n_seas = 2, con = 'by')
  dimnames_term <- list(time = 2001:2010, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rw2randomseasvary - interaction, con is 'by'", {
  prior <- RW2_Seas(n_seas = 2, con = 'by', s_seas = 0.2)
  dimnames_term <- list(time = 2001:2010, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rw2zero", {
  prior <- RW2(sd = 0)
  dimnames_term <- list(age = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- rbind(0, Matrix::.sparseDiagonal(9))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rw2zeroseasfix - interaction, con is 'by'", {
  prior <- RW2_Seas(n_seas = 2, con = 'by', sd = 0)
  dimnames_term <- list(time = 2001:2010, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_rw2zeroseasvary - interaction, con is 'by'", {
  prior <- RW2_Seas(n_seas = 2, con = 'by', s_seas = 0.2, sd = 0)
  dimnames_term <- list(time = 2001:2010, reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- make_matrix_unconstr_constr_along(c(10, 3))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - main effect", {
  prior <- Sp(n_comp = 5)
  dimnames_term <- list(age = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  expect_identical(dim(ans_obtained), c(10L, 5L))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_spline - interaction", {
  prior <- Sp(con = 'by', n_comp = 5)
  dimnames_term <- list(age = 1:10,
                        reg = 1:3)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  expect_identical(dim(ans_obtained), c(30L, 10L))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3, indep = FALSE)
  dimnames_term = list(sex = c("Female", "Male"),
                       age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][c(1,3,2,4),1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(reg = c("a", "b"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  m2 <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_matrix_perm_agesex_from_front(i_age = 2L, i_sexgender = 0L, dim_after = c(2, 2))
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd - sex x reg x age interaction", {
  prior <- SVD(HMD)
  dimnames_term <- list(sex = c("F", "M"),
                        reg = c("A", "B"),
                        age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_matrix_perm_agesex_from_front(i_age = 3L, i_sexgender = 1L, dim_after = c(2, 2, 14))
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd_ar - age-time interaction", {
  prior <- SVD_AR(HMD, n_comp = 2)
  dimnames_term = list(time = 2001:2005,
                       age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_matrix_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(dim(ans), c(prod(lengths(dimnames_term)), 10))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd_rwrandom - age-time interaction", {
  prior <- SVD_RW(HMD, n_comp = 2)
  dimnames_term = list(time = 2001:2005,
                       age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_matrix_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(dim(ans), c(prod(lengths(dimnames_term)), 10))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd_rwzero - age-time interaction", {
  prior <- SVD_RW(HMD, n_comp = 2, sd = 0)
  dimnames_term = list(time = 2001:2005,
                       age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_matrix_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(dim(ans), c(prod(lengths(dimnames_term)), 8))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd_rw2random - age-time interaction", {
  prior <- SVD_RW2(HMD, n_comp = 2)
  dimnames_term = list(time = 2001:2005,
                       age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_matrix_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(dim(ans), c(prod(lengths(dimnames_term)), 10))
})

test_that("'make_matrix_effectfree_effect' works with bage_prior_svd_rw2zero - age-time interaction", {
  prior <- SVD_RW2(HMD, n_comp = 2, sd = 0)
  dimnames_term = list(time = 2001:2005,
                       age = poputils::age_labels(type = "lt", max = 60))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_matrix_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(dim(ans), c(prod(lengths(dimnames_term)), 8))
})


## 'make_matrix_sub_orig' -----------------------------------------------------

test_that("'make_matrix_sub_orig' returns NULL with non-spline, non-SVD prior", {
  prior <- N()
  dimnames_term <- list(region = 1:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  dim_after <- 10L
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = dim_after)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig' works with 'bage_prior_spline' - main effect", {
  prior <- Sp(n = 4)
  dimnames_term <- list(age = 0:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  dim_after <- 11L
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = dim_after)
  ans_expected <- make_matrix_spline(n_comp = 4, n_along = 11)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig' works with 'bage_prior_spline' - interaction", {
  prior <- Sp(n = 4)
  dimnames_term <- list(sex = c("f", "m"),
                        age = 0:10)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  dim_after = c(2L, 11L)
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = dim_after)
  m <- make_matrix_spline(n_comp = 4, n_along = 11)
  ans_expected <- rbind(cbind(m, matrix(0, nr = 11, nc = 4)),
                        cbind(matrix(0, nr = 11, nc = 4), m))[c(1, 12, 2, 13, 3, 14, 4, 15, 5, 16,
                                                                6, 17, 7, 18, 8, 19, 9, 20, 10, 21,
                                                                11, 22),c(1,5,2,6,3,7,4,8)]
  expect_identical(as.matrix(ans_obtained), as.matrix(ans_expected))
})

test_that("'make_matrix_sub_orig' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = 2L)
  ans_expected <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3, indep = FALSE)
  dimnames_term <- list(sex = c("Female", "Male"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = c(2L, 2L))
  ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][c(1,3,2,4),1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        x = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = c(2L, 2L))
  ans_expected <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  ans_expected <- Matrix::kronecker(Matrix::.sparseDiagonal(2), ans_expected)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd - sex x reg x age interaction", {
  prior <- SVD(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"),
                       reg = c("A", "B"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = lengths(dimnames_term))
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  m1 <- make_matrix_perm_agesex_from_front(i_age = 2L,
                                           i_sexgender = 1L,
                                           dim_after = lengths(dimnames_term))
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig_svd' works with bage_prior_svd_ar - sex x time x age interaction, con is 'none'", {
  prior <- SVD_AR(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = lengths(dimnames_term))
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(10), m2)
  m1 <- make_matrix_perm_agesex_from_front(i_age = 3L,
                                           i_sexgender = 1L,
                                           dim_after = lengths(dimnames_term))
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig' works with bage_prior_svd_ar - sex x time x age interaction, con is 'by'", {
  prior <- SVD_AR(HMD, con = 'by')
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = c(1L, 10L, 13L))
  m <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m <- t(make_matrix_unconstr_constr(c(2, 14))) %*% m
  m <- Matrix::kronecker(Matrix::.sparseDiagonal(10), m)
  ans_expected <- make_matrix_perm_agesex_from_front(i_age = 3L,
                                                     i_sexgender = 1L,
                                                     dim_after = c(1, 10, 13)) %*% m
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig' works with bage_prior_svd_rwrandom - sex x time x age interaction", {
  prior <- SVD_RW(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = lengths(dimnames_term))
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(10), m2)
  m1 <- make_matrix_perm_agesex_from_front(i_age = 3L,
                                           i_sexgender = 1L,
                                           dim_after = lengths(dimnames_term))
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig' works with bage_prior_svd_rwzero - sex x time x age interaction", {
  prior <- SVD_RW(HMD, sd = 0)
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = lengths(dimnames_term))
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(10), m2)
  m1 <- make_matrix_perm_agesex_from_front(i_age = 3L,
                                           i_sexgender = 1L,
                                           dim_after = lengths(dimnames_term))
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig' works with bage_prior_svd_rw2random - sex x time x age interaction", {
  prior <- SVD_RW2(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = lengths(dimnames_term))
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(10), m2)
  m1 <- make_matrix_perm_agesex_from_front(i_age = 3L,
                                           i_sexgender = 1L,
                                           dim_after = lengths(dimnames_term))
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_sub_orig' works with bage_prior_svd_rw2zero - sex x time x age interaction", {
  prior <- SVD_RW2(HMD, sd = 0)
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_sub_orig(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender,
                                       dim_after = lengths(dimnames_term))
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(10), m2)
  m1 <- make_matrix_perm_agesex_from_front(i_age = 3L,
                                           i_sexgender = 1L,
                                           dim_after = lengths(dimnames_term))
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})


## 'make_offset_effectfree_effect' --------------------------------------------------

test_that("'make_offset_effectfree_effect' works with bage_prior_ar1", {
  prior <- AR1()
  dimnames_term <- list(time = 2001:2005)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- rep(0, 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- unname(s$data$offset[s$data$type == "total"][[1L]])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3, indep = FALSE)
  dimnames_term <- list(sex = c("Female", "Male"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect(prior = prior,
                                                dimnames_term = dimnames_term,
                                                var_time = var_time,
                                                var_age = var_age,
                                                var_sexgender = var_sexgender)
  ans_expected <- unname(s$data$offset[s$data$type == "joint"][[1L]][c(1,3,2,4)])
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd_ar - sex x time x age interaction", {
  prior <- SVD_AR1(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_offset_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(length(ans), prod(lengths(dimnames_term)))
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd_rwrandom - sex x time x age interaction", {
  prior <- SVD_RW(HMD, con = 'by')
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_offset_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(length(ans), prod(lengths(dimnames_term)))
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd_rwzero - sex x time x age interaction", {
  prior <- SVD_RW(HMD, con = 'by', sd = 0)
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_offset_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(length(ans), prod(lengths(dimnames_term)))
})

test_that("'make_offset_effectfree_effect' works with bage_prior_svd_rw2 - sex x time x age interaction", {
  prior <- SVD_RW2(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       time = 2001:2010,
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans <- make_offset_effectfree_effect(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  expect_equal(length(ans), prod(lengths(dimnames_term)))
})


## 'print' --------------------------------------------------------------------

test_that("'print' works", {
  expect_snapshot(print(AR()))
  expect_snapshot(print(AR1(min = 0.2)))
  expect_snapshot(print(Known(c(0.2, -0.2))))
  expect_snapshot(print(Lin()))
  expect_snapshot(print(Lin(s = 0)))
  expect_snapshot(print(Lin_AR()))
  expect_snapshot(print(N()))
  expect_snapshot(print(NFix()))
  expect_snapshot(print(RW()))
  expect_snapshot(print(RW_Seas(n_seas = 2)))
  expect_snapshot(print(RW_Seas(n_seas = 2, s_seas = 1)))
  expect_snapshot(print(RW(sd = 0)))
  expect_snapshot(print(RW_Seas(n_seas = 2, sd = 0)))
  expect_snapshot(print(RW_Seas(n_seas = 2, s_seas = 1, sd = 0)))
  expect_snapshot(print(RW2_Infant()))
  expect_snapshot(print(RW2()))
  expect_snapshot(print(RW2_Seas(n_seas = 2)))
  expect_snapshot(print(RW2_Seas(n_seas = 2, s_seas = 1)))
  expect_snapshot(print(RW2(sd = 0)))
  expect_snapshot(print(RW2_Seas(n_seas = 2, sd = 0)))
  expect_snapshot(print(RW2_Seas(n_seas = 2, sd = 0, s_seas = 1)))
  expect_snapshot(print(Sp()))
  expect_snapshot(print(SVD(HMD)))
  expect_snapshot(print(SVD(HMD, indep = FALSE)))
  expect_snapshot(print(SVD_AR(HMD)))
  expect_snapshot(print(SVD_AR(HMD, indep = FALSE)))
  expect_snapshot(print(SVD_AR1(HMD)))
  expect_snapshot(print(SVD_AR1(HMD, indep = FALSE)))
  expect_snapshot(print(SVD_RW(HMD)))
  expect_snapshot(print(SVD_RW(HMD, indep = FALSE)))
  expect_snapshot(print(SVD_RW(HMD, sd = 0)))
  expect_snapshot(print(SVD_RW(HMD, sd = 0, indep = FALSE)))
  expect_snapshot(print(SVD_RW2(HMD)))
  expect_snapshot(print(SVD_RW2(HMD, indep = FALSE)))
  expect_snapshot(print(SVD_RW2(HMD, sd = 0)))
  expect_snapshot(print(SVD_RW2(HMD, sd = 0, indep = FALSE)))
})


## 'str_call_prior' -----------------------------------------------------------

test_that("'str_call_prior' works with bage_prior_ar - AR1", {
    expect_identical(str_call_prior(AR1()), "AR1()")
    expect_identical(str_call_prior(AR1(min = 0.5)), "AR1(min=0.5)")
    expect_identical(str_call_prior(AR1(max = 0.95)),"AR1(max=0.95)")
    expect_identical(str_call_prior(AR1(s = 0.3)), "AR1(s=0.3)")
    expect_identical(str_call_prior(AR1(min = 0.5, max = 0.95, along = "age",
                                        s = 0.3, con = 'by', shape2 = 3,
                                        shape1 = 2)),
                     "AR1(s=0.3,shape1=2,shape2=3,min=0.5,max=0.95,along=\"age\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_ar - AR", {
    expect_identical(str_call_prior(AR(n_coef = 1)), "AR(n_coef=1)")
    expect_identical(str_call_prior(AR(n_coef = 3, s = 0.3)), "AR(n_coef=3,s=0.3)")
    expect_identical(str_call_prior(AR(s = 0.3, shape1 = 2, shape2 = 2,
                                       con = "by", along = "cohort", n = 2)),
                     "AR(s=0.3,shape1=2,shape2=2,along=\"cohort\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_known", {
    expect_identical(str_call_prior(Known(1)), "Known(1)")
    expect_identical(str_call_prior(Known(c(2, 3, -2, 0))),
                                    "Known(c(2,3,-2,0))")
    expect_identical(str_call_prior(Known(c(2, 3, -2, 0,7, 3))),
                                    "Known(c(2,...,3))")
})

test_that("'str_call_prior' works with bage_prior_lin", {
    expect_identical(str_call_prior(Lin()), "Lin()")
    expect_identical(str_call_prior(Lin(sd_slope = 0.5, mean_slope = -0.2, s = 2,
                                        con = 'by', along = "a")),
                     "Lin(s=2,mean_slope=-0.2,sd_slope=0.5,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_linar - AR format", {
    expect_identical(str_call_prior(Lin_AR()), "Lin_AR()")
    expect_identical(str_call_prior(Lin_AR(sd = 0.5)), "Lin_AR(sd_slope=0.5)")
    expect_identical(str_call_prior(Lin_AR(sd=2L,mean_slope=0.1,s = 0.95,
                                           shape1 = 2, shape2 = 2)),
                     "Lin_AR(s=0.95,shape1=2,shape2=2,mean_slope=0.1,sd_slope=2)")
    expect_identical(str_call_prior(Lin_AR(sd = 0.1, along = "cohort", con = "by",
                                           s = 0.95,n=3)),
                     "Lin_AR(n_coef=3,s=0.95,sd_slope=0.1,along=\"cohort\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_linar - AR1 format", {
    expect_identical(str_call_prior(Lin_AR1()), "Lin_AR1()")
    expect_identical(str_call_prior(Lin_AR1(along="age",sd = 0.5)),
                     "Lin_AR1(sd_slope=0.5,along=\"age\")")
    expect_identical(str_call_prior(Lin_AR1(sd=2L,s = 0.95)), "Lin_AR1(s=0.95,sd_slope=2)")
    expect_identical(str_call_prior(Lin_AR1(sd = 0.1, mean_slope = 0.2,
                                            con = "by", max=1,s = 0.95, min = 0.5,
                                            shape1 = 1, shape2 = 0.1)),
                     "Lin_AR1(s=0.95,shape1=1,shape2=0.1,min=0.5,max=1,mean_slope=0.2,sd_slope=0.1,con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_linex", {
    expect_identical(str_call_prior(Lin(s = 0)), "Lin(s=0)")
    expect_identical(str_call_prior(Lin(sd_slope = 0.5, mean_slope = -0.2, s = 0,
                                        con = 'by', along = "a")),
                     "Lin(s=0,mean_slope=-0.2,sd_slope=0.5,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_norm", {
    expect_identical(str_call_prior(N()), "N()")
    expect_identical(str_call_prior(N(s = 0.95)), "N(s=0.95)")
})

test_that("'str_call_prior' works with bage_prior_normfixed", {
    expect_identical(str_call_prior(NFix()), "NFix()")
    expect_identical(str_call_prior(NFix(sd = 0.95)), "NFix(sd=0.95)")
})

test_that("'str_call_prior' works with bage_prior_rwrandom", {
    expect_identical(str_call_prior(RW()), "RW()")
    expect_identical(str_call_prior(RW(along = "a", s = 2, sd = 0.2, con="by")),
                     "RW(s=2,sd=0.2,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rwrandomseasfix", {
    expect_identical(str_call_prior(RW_Seas(n_seas=2)), "RW_Seas(n_seas=2)")
    expect_identical(str_call_prior(RW_Seas(along = "a", s = 2, n = 5, sd_seas = 2.0,
                                            con="by",sd=0.2)),
                     "RW_Seas(n_seas=5,s=2,sd=0.2,sd_seas=2,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rwrandomseasvary", {
    expect_identical(str_call_prior(RW_Seas(n_seas=2)), "RW_Seas(n_seas=2)")
    expect_identical(str_call_prior(RW_Seas(along = "a", sd_seas = 3.00, s = 2,
                                            sd=0.2,con = "by", n = 5, s_seas=0.1)),
                     "RW_Seas(n_seas=5,s=2,sd=0.2,s_seas=0.1,sd_seas=3,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rwzero", {
    expect_identical(str_call_prior(RW(sd = 0)), "RW(sd=0)")
    expect_identical(str_call_prior(RW(along = "a", s = 2, sd = 0, con="by")),
                     "RW(s=2,sd=0,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rwzeroseasfix", {
    expect_identical(str_call_prior(RW_Seas(n_seas=2,sd = 0)), "RW_Seas(n_seas=2,sd=0)")
    expect_identical(str_call_prior(RW_Seas(along = "a", s = 2, n = 5, sd_seas = 2.0,
                                            con="by",sd=0)),
                     "RW_Seas(n_seas=5,s=2,sd=0,sd_seas=2,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rwzeroseasvary", {
    expect_identical(str_call_prior(RW_Seas(n_seas=2,sd=0)), "RW_Seas(n_seas=2,sd=0)")
    expect_identical(str_call_prior(RW_Seas(along = "a", sd_seas = 3.00, s = 2,
                                            sd=0,con = "by", n = 5, s_seas=0.1)),
                     "RW_Seas(n_seas=5,s=2,sd=0,s_seas=0.1,sd_seas=3,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rw2infant", {
  expect_identical(str_call_prior(RW2_Infant()), "RW2_Infant()")
  expect_identical(str_call_prior(RW2_Infant(s = 2, con = 'by')),
                   "RW2_Infant(s=2,con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rw2random", {
  expect_identical(str_call_prior(RW2()), "RW2()")
  expect_identical(str_call_prior(RW2(along = "a", s = 2, sd = 0.5, con = 'by')),
                   "RW2(s=2,sd=0.5,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rw2randomseasfix", {
  expect_identical(str_call_prior(RW2_Seas(n_seas=2, s_seas = 0)),
                   "RW2_Seas(n_seas=2)")
  expect_identical(str_call_prior(RW2_Seas(along = "a", sd_seas = 3, s = 2, sd = 0.2,
                                           con = 'by', n = 5)),
                     "RW2_Seas(n_seas=5,s=2,sd=0.2,sd_seas=3,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rw2randomseasvary", {
    expect_identical(str_call_prior(RW2_Seas(n_seas=2)), "RW2_Seas(n_seas=2)")
    expect_identical(str_call_prior(RW2_Seas(along = "a",
                                             s = 2, n = 5, s_seas=0.1,
                                             con="by",sd_seas = 0.2)),
                     "RW2_Seas(n_seas=5,s=2,s_seas=0.1,sd_seas=0.2,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rw2zero", {
  expect_identical(str_call_prior(RW2(sd = 0)), "RW2(sd=0)")
  expect_identical(str_call_prior(RW2(along = "a", s = 2, sd = 0, con = 'by')),
                   "RW2(s=2,sd=0,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rw2zeroseasfix", {
  expect_identical(str_call_prior(RW2_Seas(n_seas=2, sd = 0, s_seas = 0)),
                   "RW2_Seas(n_seas=2,sd=0)")
  expect_identical(str_call_prior(RW2_Seas(along = "a", sd_seas = 3, s = 2, sd = 0,
                                           con = 'by', n = 5)),
                     "RW2_Seas(n_seas=5,s=2,sd=0,sd_seas=3,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_rw2zeroseasvary", {
    expect_identical(str_call_prior(RW2_Seas(sd=0,n_seas=2)), "RW2_Seas(n_seas=2,sd=0)")
    expect_identical(str_call_prior(RW2_Seas(along = "a",
                                             s = 2, n = 5, s_seas=0.1, sd=0,
                                             con="by",sd_seas = 0.2)),
                     "RW2_Seas(n_seas=5,s=2,sd=0,s_seas=0.1,sd_seas=0.2,along=\"a\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_spline", {
    expect_identical(str_call_prior(Sp()), "Sp()")
    expect_identical(str_call_prior(Sp(n = 5L)), "Sp(n_comp=5)")
    expect_identical(str_call_prior(Sp(s = 0.1, sd = 0.5)), "Sp(s=0.1,sd=0.5)")
    expect_identical(str_call_prior(Sp(s = 3,along = "cohort", n = 5L,
                                       con="by",sd=2.0000)),
                     "Sp(n_comp=5,s=3,sd=2,along=\"cohort\",con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_svd", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(SVD(s)), "SVD(s)")
    expect_identical(str_call_prior(SVD(s,indep = FALSE)), "SVD(s,indep=FALSE)")
    expect_identical(str_call_prior(SVD(s,n = 6L)), "SVD(s,n_comp=6)")
    expect_identical(str_call_prior(SVD(s,indep=T,n_comp = 3L)),
                     "SVD(s,n_comp=3)")
})

test_that("'str_call_prior' works with bage_prior_svd_ar", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(SVD_AR1(s)), "SVD_AR1(s)")
    expect_identical(str_call_prior(SVD_AR1(s,indep = FALSE)), "SVD_AR1(s,indep=FALSE)")
    expect_identical(str_call_prior(SVD_AR1(s,min = 0.2, n = 6L)), "SVD_AR1(s,n_comp=6,min=0.2)")
    expect_identical(str_call_prior(SVD_AR(s,indep=T,n_comp = 3L,n_coef=3,con="by")),
                     "SVD_AR(s,n_comp=3,n_coef=3,con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_svd_rwrandom", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(SVD_RW(s)), "SVD_RW(s)")
    expect_identical(str_call_prior(SVD_RW(s,indep = FALSE)), "SVD_RW(s,indep=FALSE)")
    expect_identical(str_call_prior(SVD_RW(s,indep = TRUE,con="by",
                                           n_comp = 3L,sd=3,s=0.3)),
                     "SVD_RW(s,n_comp=3,s=0.3,sd=3,con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_svd_rwzero", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(SVD_RW(s,sd = 0)), "SVD_RW(s,sd=0)")
    expect_identical(str_call_prior(SVD_RW(s,indep = FALSE, sd = 0)), "SVD_RW(s,indep=FALSE,sd=0)")
    expect_identical(str_call_prior(SVD_RW(s,indep = TRUE,con="by",
                                           n_comp = 3L,s=0.3,sd=0)),
                     "SVD_RW(s,n_comp=3,s=0.3,sd=0,con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_svd_rw2random", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(SVD_RW2(s)), "SVD_RW2(s)")
    expect_identical(str_call_prior(SVD_RW2(s,indep=F,sd=0.1)), "SVD_RW2(s,indep=FALSE,sd=0.1)")
    expect_identical(str_call_prior(SVD_RW2(s,indep=T,sd_slope=0.2,n_comp = 3L,s=0.3,sd=2,con="by")),
                     "SVD_RW2(s,n_comp=3,s=0.3,sd=2,sd_slope=0.2,con=\"by\")")
})

test_that("'str_call_prior' works with bage_prior_svd_rw2zero", {
    s <- sim_ssvd()
    expect_identical(str_call_prior(SVD_RW2(s,sd=0)), "SVD_RW2(s,sd=0)")
    expect_identical(str_call_prior(SVD_RW2(s,indep=F,sd=0)), "SVD_RW2(s,indep=FALSE,sd=0)")
    expect_identical(str_call_prior(SVD_RW2(s,indep=T,sd_slope=0.2,sd=0,n_comp = 3L,s=0.3,con="by")),
                     "SVD_RW2(s,n_comp=3,s=0.3,sd=0,sd_slope=0.2,con=\"by\")")
})


## 'str_nm_prior' -----------------------------------------------------------

test_that("'str_nm_prior' works with bage_prior_ar - AR1", {
  expect_identical(str_nm_prior(AR1(s = 3)), "AR1()")
})

test_that("'str_nm_prior' works with bage_prior_ar - AR", {
   expect_identical(str_nm_prior(AR(n_coef = 1)), "AR()")
   expect_identical(str_nm_prior(AR(n_coef = 3, s = 0.3)), "AR()")
})

test_that("'str_nm_prior' works with bage_prior_known", {
    expect_identical(str_nm_prior(Known(1)), "Known()")
    expect_identical(str_nm_prior(Known(c(2, 3, -2, 0,7, 3))), "Known()")
})

test_that("'str_nm_prior' works with bage_prior_lin", {
    expect_identical(str_nm_prior(Lin()), "Lin()")
    expect_identical(str_nm_prior(Lin(sd = 0.1, s = 0.95)), "Lin()")
})

test_that("'str_nm_prior' works with bage_prior_linar - AR", {
   expect_identical(str_nm_prior(Lin_AR(n_coef = 1)), "Lin_AR()")
   expect_identical(str_nm_prior(Lin_AR(n_coef = 3, s = 0.3)), "Lin_AR()")
})

test_that("'str_nm_prior' works with bage_prior_linar - AR1", {
   expect_identical(str_nm_prior(Lin_AR1(max = 1)), "Lin_AR1()")
   expect_identical(str_nm_prior(Lin_AR1(sd = 3, s = 0.3)), "Lin_AR1()")
})

test_that("'str_nm_prior' works with bage_prior_linex", {
   expect_identical(str_nm_prior(Lin(s = 0)), "Lin()")
   expect_identical(str_nm_prior(Lin(s = 0, sd_slope = 3)), "Lin()")
})

test_that("'str_nm_prior' works with bage_prior_norm", {
    expect_identical(str_nm_prior(N()), "N()")
    expect_identical(str_nm_prior(N(s = 0.95)), "N()")
})

test_that("'str_nm_prior' works with bage_prior_normfixed", {
    expect_identical(str_nm_prior(NFix()), "NFix()")
    expect_identical(str_nm_prior(NFix(sd = 0.95)), "NFix()")
})

test_that("'str_nm_prior' works with bage_prior_rwrandom", {
    expect_identical(str_nm_prior(RW()), "RW()")
    expect_identical(str_nm_prior(RW(s = 0.95)), "RW()")
})

test_that("'str_nm_prior' works with bage_prior_rwrandomseasfix", {
    expect_identical(str_nm_prior(RW_Seas(n_seas = 3)), "RW_Seas()")
    expect_identical(str_nm_prior(RW_Seas(n_seas = 3, s = 0.95, s_seas = 0)), "RW_Seas()")
})

test_that("'str_nm_prior' works with bage_prior_rwrandomseasvary", {
    expect_identical(str_nm_prior(RW_Seas(n_seas = 3, s_seas = 4)), "RW_Seas()")
    expect_identical(str_nm_prior(RW_Seas(n_seas = 3, s = 0.95)), "RW_Seas()")
})

test_that("'str_nm_prior' works with bage_prior_rwzero", {
    expect_identical(str_nm_prior(RW(sd = 0)), "RW()")
    expect_identical(str_nm_prior(RW(s = 0.95, sd = 0)), "RW()")
})

test_that("'str_nm_prior' works with bage_prior_rwzeroseasfix", {
    expect_identical(str_nm_prior(RW_Seas(n_seas = 3, sd = 0)), "RW_Seas()")
    expect_identical(str_nm_prior(RW_Seas(n_seas = 3, s = 0.95, sd = 0, s_seas = 0)), "RW_Seas()")
})

test_that("'str_nm_prior' works with bage_prior_rwzeroseasvary", {
    expect_identical(str_nm_prior(RW_Seas(n_seas = 3, sd = 0,  s_seas = 4)), "RW_Seas()")
    expect_identical(str_nm_prior(RW_Seas(n_seas = 3, s = 0.95, sd = 0)), "RW_Seas()")
})

test_that("'str_nm_prior' works with bage_prior_rw2infant", {
    expect_identical(str_nm_prior(RW2_Infant()), "RW2_Infant()")
    expect_identical(str_nm_prior(RW2_Infant(s = 0.95)), "RW2_Infant()")
})

test_that("'str_nm_prior' works with bage_prior_rw2random", {
    expect_identical(str_nm_prior(RW2()), "RW2()")
    expect_identical(str_nm_prior(RW2(s = 0.95)), "RW2()")
})

test_that("'str_nm_prior' works with bage_prior_rw2randomseasfix", {
    expect_identical(str_nm_prior(RW2_Seas(n_seas = 3, s_seas = 0)), "RW2_Seas()")
    expect_identical(str_nm_prior(RW2_Seas(n_seas = 3, s = 0.95, s_seas = 0)), "RW2_Seas()")
})

test_that("'str_nm_prior' works with bage_prior_rw2randomseasvary", {
    expect_identical(str_nm_prior(RW2_Seas(n_seas = 3, s_seas = 4)), "RW2_Seas()")
    expect_identical(str_nm_prior(RW2_Seas(n_seas = 3, s = 0.95)), "RW2_Seas()")
})

test_that("'str_nm_prior' works with bage_prior_rw2zero", {
    expect_identical(str_nm_prior(RW2(sd = 0)), "RW2()")
    expect_identical(str_nm_prior(RW2(sd = 0, s = 0.95)), "RW2()")
})

test_that("'str_nm_prior' works with bage_prior_rw2zeroseasfix", {
    expect_identical(str_nm_prior(RW2_Seas(n_seas = 3, s_seas = 0, sd = 0)), "RW2_Seas()")
    expect_identical(str_nm_prior(RW2_Seas(n_seas = 3, s = 0.95, sd = 0, s_seas = 0)), "RW2_Seas()")
})

test_that("'str_nm_prior' works with bage_prior_rw2zeroseasvary", {
    expect_identical(str_nm_prior(RW2_Seas(n_seas = 3, s_seas = 4, sd = 0)), "RW2_Seas()")
    expect_identical(str_nm_prior(RW2_Seas(n_seas = 3, s = 0.95, sd = 0)), "RW2_Seas()")
})

test_that("'str_nm_prior' works with bage_prior_spline", {
    expect_identical(str_nm_prior(Sp()), "Sp()")
    expect_identical(str_nm_prior(Sp(s = 3,n = 5L)), "Sp()")
})

test_that("'str_nm_prior' works with bage_prior_svd", {
    s <- sim_ssvd()
    expect_identical(str_nm_prior(SVD(s)), "SVD()")
    expect_identical(str_nm_prior(SVD(s,indep=T,n = 3L)), "SVD()")
})

test_that("'str_nm_prior' works with bage_prior_svd_ar", {
    s <- sim_ssvd()
    expect_identical(str_nm_prior(SVD_AR(s)), "SVD_AR()")
    expect_identical(str_nm_prior(SVD_AR1(s,indep=T,n = 3L)), "SVD_AR1()")
})

test_that("'str_nm_prior' works with bage_prior_svd_rwrandom", {
    s <- sim_ssvd()
    expect_identical(str_nm_prior(SVD_RW(s)), "SVD_RW()")
    expect_identical(str_nm_prior(SVD_RW(s,indep=T)), "SVD_RW()")
})

test_that("'str_nm_prior' works with bage_prior_svd_rwzero", {
    s <- sim_ssvd()
    expect_identical(str_nm_prior(SVD_RW(s, sd = 0)), "SVD_RW()")
    expect_identical(str_nm_prior(SVD_RW(s,indep=T, sd = 0)), "SVD_RW()")
})

test_that("'str_nm_prior' works with bage_prior_svd_rw2random", {
    s <- sim_ssvd()
    expect_identical(str_nm_prior(SVD_RW2(s)), "SVD_RW2()")
    expect_identical(str_nm_prior(SVD_RW2(s,indep=T)), "SVD_RW2()")
})

test_that("'str_nm_prior' works with bage_prior_svd_rw2zero", {
    s <- sim_ssvd()
    expect_identical(str_nm_prior(SVD_RW2(s, sd = 0)), "SVD_RW2()")
    expect_identical(str_nm_prior(SVD_RW2(s,indep=T, sd = 0)), "SVD_RW2()")
})


## transform_hyper ------------------------------------------------------------

test_that("'transform_hyper' works with 'bage_prior_ar - AR1'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper(prior = AR1())
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_ar - AR'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper(prior = AR(n_coef = 2))
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_known'", {
  l <- transform_hyper(prior = Known(1))
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_lin'", {
  l <- transform_hyper(prior = Lin())
  expect_equal(exp(0.35), l[[1]](0.35))
})

test_that("'transform_hyper' works with 'bage_prior_linar - AR'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper(prior = Lin_AR(n_coef = 2))
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_linar - AR1'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper(prior = Lin_AR1())
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_linex'", {
  l <- transform_hyper(prior = Lin(s = 0))
  expect_equal(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_norm'", {
  l <- transform_hyper(prior = N())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_normfixed'", {
  l <- transform_hyper(prior = NFix())
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_rwrandom'", {
  l <- transform_hyper(prior = RW())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rwrandomseasfix'", {
  l <- transform_hyper(prior = RW_Seas(n_seas = 3))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rwrandomseasvary'", {
  l <- transform_hyper(prior = RW_Seas(n_seas = 3, s_seas = 1))
  expect_equal(0.35, l[[1]](log(0.35)))
  expect_equal(0.35, l[[2]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rwzero'", {
  l <- transform_hyper(prior = RW(sd = 0))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rwzeroseasfix'", {
  l <- transform_hyper(prior = RW_Seas(n_seas = 3, sd = 0))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rwzeroseasvary'", {
  l <- transform_hyper(prior = RW_Seas(n_seas = 3, s_seas = 1, sd = 0))
  expect_equal(0.35, l[[1]](log(0.35)))
  expect_equal(0.35, l[[2]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2infant'", {
  l <- transform_hyper(prior = RW2_Infant())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2random'", {
  l <- transform_hyper(prior = RW2())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2randomseasfix'", {
  l <- transform_hyper(prior = RW2_Seas(n_seas = 3))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2randomseasvary'", {
  l <- transform_hyper(prior = RW2_Seas(n_seas = 3, s_seas = 1))
  expect_equal(0.35, l[[1]](log(0.35)))
  expect_equal(0.35, l[[2]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2zero'", {
  l <- transform_hyper(prior = RW2(sd = 0))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2zeroseasfix'", {
  l <- transform_hyper(prior = RW2_Seas(n_seas = 3, sd = 0))
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_rw2zeroseasvary'", {
  l <- transform_hyper(prior = RW2_Seas(n_seas = 3, s_seas = 1, sd = 0))
  expect_equal(0.35, l[[1]](log(0.35)))
  expect_equal(0.35, l[[2]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_spline'", {
  l <- transform_hyper(prior = Sp())
  expect_equal(0.35, l[[1]](log(0.35)))
})

test_that("'transform_hyper' works with 'bage_prior_svd'", {
  l <- transform_hyper(prior = SVD(HMD))
  expect_identical(l, list())
})

test_that("'transform_hyper' works with 'bage_prior_svd_ar - AR1'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.8 + ans * 0.18
  }
  l <- transform_hyper_ar(prior = SVD_AR1(HMD))
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_svd_rwrandom'", {
  l <- transform_hyper_ar(prior = SVD_RW(LFP))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_svd_rwzero'", {
  l <- transform_hyper_ar(prior = SVD_RW(LFP, sd = 0))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_svd_rw2random'", {
  l <- transform_hyper_ar(prior = SVD_RW2(LFP))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper' works with 'bage_prior_svd_rw2zero'", {
  l <- transform_hyper_ar(prior = SVD_RW2(LFP, sd = 0))
  expect_equal(l[[2]](0.35), exp(0.35))
})


## uses_along -----------------------------------------------------------------

test_that("'uses_along' works with valid inputs", {
  expect_true(uses_along(AR()))
  expect_false(uses_along(Known(c(a = 1))))
  expect_true(uses_along(Lin()))
  expect_true(uses_along(Lin_AR()))
  expect_true(uses_along(Lin(s = 0)))
  expect_false(uses_along(N()))
  expect_false(uses_along(NFix()))
  expect_true(uses_along(RW()))
  expect_true(uses_along(RW_Seas(n_seas = 3)))
  expect_true(uses_along(RW_Seas(n_seas = 3, s_seas = 1)))
  expect_true(uses_along(RW(sd = 0)))
  expect_true(uses_along(RW_Seas(n_seas = 3, sd = 0)))
  expect_true(uses_along(RW_Seas(n_seas = 3, s_seas = 1, sd = 0)))
  expect_true(uses_along(RW2_Infant()))
  expect_true(uses_along(RW2()))
  expect_true(uses_along(RW2_Seas(n_seas = 3)))
  expect_true(uses_along(RW2_Seas(n_seas = 3, s_seas = 1)))
  expect_true(uses_along(RW2(sd = 0)))
  expect_true(uses_along(RW2_Seas(n_seas = 3, sd = 0)))
  expect_true(uses_along(RW2_Seas(n_seas = 3, s_seas = 1, sd = 0)))
  expect_true(uses_along(Sp()))
  expect_false(uses_along(SVD(HMD)))
  expect_true(uses_along(SVD_AR(HMD)))
  expect_true(uses_along(SVD_RW(HMD)))
  expect_true(uses_along(SVD_RW(HMD, sd = 0)))
  expect_true(uses_along(SVD_RW2(HMD)))
  expect_true(uses_along(SVD_RW2(HMD, sd = 0)))
})


## uses_hyperrandfree ------------------------------------------------------

test_that("'uses_hyperrandfree' returns FALSE with priors that do not use hyperrandfree parameters", {
  expect_false(uses_hyperrandfree(AR1()))
  expect_false(uses_hyperrandfree(Known(c(a = 1, b = -1))))
  expect_false(uses_hyperrandfree(Lin(s = 0)))
  expect_false(uses_hyperrandfree(N()))
  expect_false(uses_hyperrandfree(NFix()))
  expect_false(uses_hyperrandfree(RW()))
  expect_false(uses_hyperrandfree(RW(sd = 0)))
  expect_false(uses_hyperrandfree(RW2()))
  expect_false(uses_hyperrandfree(RW2(sd = 0)))
  expect_false(uses_hyperrandfree(Sp()))
  expect_false(uses_hyperrandfree(SVD(HMD)))
  expect_false(uses_hyperrandfree(SVD_AR(HMD)))
  expect_false(uses_hyperrandfree(SVD_RW(HMD)))
  expect_false(uses_hyperrandfree(SVD_RW2(HMD)))
})

test_that("'uses_hyperrandfree' returns TRUE with priors do use hyperrandfree parameters", {
  expect_true(uses_hyperrandfree(Lin()))
  expect_true(uses_hyperrandfree(Lin_AR()))
  expect_true(uses_hyperrandfree(RW_Seas(n_seas = 3)))
  expect_true(uses_hyperrandfree(RW_Seas(n_seas = 3, s_seas = 1)))
  expect_true(uses_hyperrandfree(RW_Seas(n_seas = 3, sd = 0)))
  expect_true(uses_hyperrandfree(RW_Seas(n_seas = 3, s_seas = 1, sd = 0)))
  expect_true(uses_hyperrandfree(RW2_Seas(n_seas = 3)))
  expect_true(uses_hyperrandfree(RW2_Seas(n_seas = 3, s_seas = 1)))
  expect_true(uses_hyperrandfree(RW2_Seas(n_seas = 3, sd = 0)))
  expect_true(uses_hyperrandfree(RW2_Seas(n_seas = 3, s_seas = 1, sd = 0)))
})


## uses_matrix_effectfree_effect ----------------------------------------------

test_that("'uses_matrix_effectfree_effect' works with valid inputs", {
  expect_false(uses_matrix_effectfree_effect(N()))
  expect_false(uses_matrix_effectfree_effect(Lin()))
  expect_true(uses_matrix_effectfree_effect(Lin(con = "by")))
  expect_true(uses_matrix_effectfree_effect(Lin(s=0)))
  expect_false(uses_matrix_effectfree_effect(Lin_AR()))
  expect_true(uses_matrix_effectfree_effect(Lin_AR(con = "by")))
  expect_true(uses_matrix_effectfree_effect(RW()))
  expect_true(uses_matrix_effectfree_effect(RW_Seas(n = 2)))
  expect_true(uses_matrix_effectfree_effect(RW_Seas(n = 2, s_seas = 1)))
  expect_true(uses_matrix_effectfree_effect(RW(sd = 0)))
  expect_true(uses_matrix_effectfree_effect(RW_Seas(n = 2, sd = 0)))
  expect_true(uses_matrix_effectfree_effect(RW_Seas(n = 2, sd = 0, s_seas = 1)))
  expect_true(uses_matrix_effectfree_effect(RW2_Infant()))
  expect_true(uses_matrix_effectfree_effect(RW2()))
  expect_true(uses_matrix_effectfree_effect(RW2_Seas(n = 2)))
  expect_true(uses_matrix_effectfree_effect(RW2_Seas(n = 2, s_seas = 1)))
  expect_true(uses_matrix_effectfree_effect(RW2(sd = 0)))
  expect_true(uses_matrix_effectfree_effect(RW2_Seas(n = 2, sd = 0)))
  expect_true(uses_matrix_effectfree_effect(RW2_Seas(n = 2, s_seas = 1, sd = 0)))
  expect_true(uses_matrix_effectfree_effect(Sp()))
  expect_true(uses_matrix_effectfree_effect(SVD(HMD)))
  expect_true(uses_matrix_effectfree_effect(SVD_AR(HMD)))
  expect_true(uses_matrix_effectfree_effect(SVD_RW(HMD)))
  expect_true(uses_matrix_effectfree_effect(SVD_RW(HMD, sd = 0)))
  expect_true(uses_matrix_effectfree_effect(SVD_RW2(HMD)))
  expect_true(uses_matrix_effectfree_effect(SVD_RW2(HMD, sd = 0)))
})


## uses_offset_effectfree_effect ----------------------------------------------

test_that("'uses_offset_effectfree_effect' works with valid inputs", {
  expect_false(uses_offset_effectfree_effect(N()))
  expect_false(uses_offset_effectfree_effect(Sp()))
  expect_true(uses_offset_effectfree_effect(SVD(HMD)))
  expect_true(uses_offset_effectfree_effect(SVD_AR(HMD)))
  expect_true(uses_offset_effectfree_effect(SVD_RW(HMD)))
  expect_true(uses_offset_effectfree_effect(SVD_RW(HMD, sd = 0)))
  expect_true(uses_offset_effectfree_effect(SVD_RW2(HMD)))
  expect_true(uses_offset_effectfree_effect(SVD_RW2(HMD, sd = 0)))
})


## values_known ---------------------------------------------------------------

test_that("'values_known' works with valid inputs", {
    expect_identical(values_known(Known(values = 1:3)),
                     as.double(1:3))
})
