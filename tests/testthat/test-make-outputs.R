
## 'combine_stored_draws_point_inner_outer' -----------------------------------------

test_that("'combine_stored_draws_point_inner_outer' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:4,
                      sex = c("F", "M"),
                      region = c("a", "b"),
                      time = 2001:2005)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex  + region * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, region:time ~ Lin())
  mod <- set_n_draw(mod, n_draw = 10)
  vars_inner <- c("age", "sex")
  use_term <- make_use_term(mod = mod, vars_inner = vars_inner)
  mod_inner <- reduce_model_terms(mod = mod, use_term = use_term)
  mod_inner <- fit_default(mod_inner, optimizer = "nlminb", quiet = TRUE, aggregate = TRUE,
                           start_oldpar = FALSE)
  mod_outer <- reduce_model_terms(mod = mod, use_term = !use_term)
  mod_outer <- fit_default(mod_outer, optimizer = "nlminb", quiet = TRUE, aggregate = TRUE,
                           start_oldpar = FALSE)
  mod_comb <- combine_stored_draws_point_inner_outer(mod = mod,
                                                     mod_inner = mod_inner,
                                                     mod_outer = mod_outer,
                                                     use_term = use_term)
  terms <- make_terms_effectfree(mod)
  is_inner <- terms %in% c("(Intercept)", "age", "sex", "age:sex")
  is_outer <- terms %in% c("region", "time", "region:time")
  expect_identical(mod_comb$draws_effectfree[is_inner, ],
                   mod_inner$draws_effectfree)
  expect_identical(mod_comb$draws_effectfree[is_outer, ],
                   mod_outer$draws_effectfree)
  expect_identical(ncol(mod_comb$draws_hyper),
                   ncol(mod_outer$draws_hyper))
  expect_identical(ncol(mod_comb$draws_hyperrand),
                   ncol(mod_outer$draws_hyperrand))
  expect_identical(mod_comb$point_effectfree[is_inner],
                   mod_inner$point_effectfree)
  expect_identical(mod_comb$point_effectfree[is_outer],
                   mod_outer$point_effectfree)
})


## 'con_by_fitted' ------------------------------------------------------------

test_that("'con_by_fitted' works", {
  set.seed(0)
  prior <- RW()
  fitted <- rvec::rnorm_rvec(n = 100, n_draw = 10)
  along <- "time"
  dimnames_term <- list(time = 2001:2010,
                        age = 0:4,
                        sex = 1:2)
  var_time <- "time"
  var_age <- "age"
  ans <- con_by_fitted(prior = prior,
                         fitted = fitted,
                         dimnames_term = dimnames_term,
                         var_time = var_time,
                         var_age = var_age)
  expect_equal(sum(ans[c(3, 13, 23, 33, 43)]),
               sum(ans[c(4, 14, 24, 34, 44)]))
  expect_equal(ans[10], -ans[60])
})


## 'draw_vals_components_fitted' ----------------------------------------------

test_that("'draw_vals_components_fitted' works", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp())
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- fit(mod)
  set.seed(0)
  ans <- draw_vals_components_fitted(mod)
  expect_identical(names(ans), c("term", "component", "level", ".fitted"))
  i <- ans$term == "age:sex" & ans$component == "effect"
  expect_true(mean(abs(as.numeric(sum(ans$.fitted[i])))) > 0)
})


## 'generate_prior_helper' ----------------------------------------------------

test_that("'generate_prior_helper' works with valid inputs, not along-by", {
  x <- N()
  ans_obtained <- generate_prior_helper(x = x, n_element = 5, n_draw = 2)
  draw <- factor(rep(c("Draw 1", "Draw 2"), each = 5))
  ans_expected <- list(ans = tibble::tibble(draw = draw,
                                            element = rep(1:5, times = 2)),
                       matrix_along_by = matrix(0:4, nc = 1, dimnames = list(seq_len(5), NULL)),
                       levels_effect = 1:5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'generate_prior_helper' works with valid inputs, along-by, n_by = 1", {
  x <- AR()
  ans_obtained <- generate_prior_helper(x = x, n_along = 5, n_by = 1, n_draw = 2)
  draw <- factor(rep(c("Draw 1", "Draw 2"), each = 5))
  by <- factor(rep("By 1", 10))
  levels_effect <- paste(1, 1:5, sep = ".")
  ans_expected <- list(ans = tibble::tibble(draw = draw,
                                            by = by,
                                            along = rep(1:5, times = 2)),
                       matrix_along_by = matrix(0:4, nc = 1, dimnames = list(seq_len(5), 1L)),
                       levels_effect = levels_effect)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'generate_prior_helper' works with valid inputs, along-by, n_by = 2", {
  x <- AR()
  ans_obtained <- generate_prior_helper(x = x, n_along = 5, n_by = 2, n_draw = 2)
  draw <- factor(rep(c("Draw 1", "Draw 2"), each = 10))
  by <- factor(rep(rep(paste("By", 1:2), each = 5), times = 2))
  ans_expected <- list(ans = tibble::tibble(draw = draw,
                                            by = by,
                                            along = rep(1:5, times = 4)),
                       matrix_along_by = matrix(0:9, nc = 2, dimnames = list(1:5, 1:2)),
                       levels_effect = paste(rep(1:2, each = 5), 1:5, sep = "."))
  expect_identical(ans_obtained, ans_expected)
})


## 'generate_prior_svd_helper' ------------------------------------------------

test_that("'generate_prior_svd_helper' works with valid inputs - n_element", {
  x <- SVD(LFP)
  set.seed(0)
  n_element <- 20
  n_draw <- 25
  ans_obtained <- generate_prior_svd_helper(x, n_element = n_element, n_draw = n_draw)
  expect_true(is.list(ans_obtained))
})

test_that("'generate_prior_svd_helper' works with valid inputs - n_by = 1", {
  x <- SVD_RW(LFP)
  set.seed(0)
  n_along <- 20
  n_by <- 2
  n_draw <- 25
  ans_obtained <- generate_prior_svd_helper(x, n_along = n_along, n_by = n_by, n_draw = n_draw)
  expect_true(is.list(ans_obtained))
})


## 'generate_ssvd_helper' -----------------------------------------------------------------

test_that("'generate_ssvd_helper' works with valid inputs - indep = NULL, n_element = 1", {
  set.seed(0)
  ans <- generate_ssvd_helper(ssvd = LFP,
                              n_element = 1,
                              n_draw = 3,
                              n_comp = 2,
                              indep = NULL,
                              age_labels = NULL)
  expect_identical(nrow(ans$matrix), length(unique(ans$ans$age)))
  expect_identical(ncol(ans$matrix), 2L)
})

test_that("'generate_ssvd_helper' works with valid inputs - indep = TRUE, n_by = 1, n_along = 3", {
  set.seed(0)
  ans <- generate_ssvd_helper(ssvd = LFP,
                              n_along = 3,
                              n_by = 1,
                              n_draw = 3,
                              n_comp = 2,
                              indep = TRUE,
                              age_labels = NULL)
  expect_identical(nrow(ans$matrix), 3L * nrow(unique(ans$ans[c("age", "sexgender")])))
  expect_identical(ncol(ans$matrix), 12L)
})

test_that("'generate_ssvd_helper' works with valid inputs - indep = FALSE, n_by = 1", {
  set.seed(0)
  ans <- generate_ssvd_helper(ssvd = LFP,
                              n_along = 2,
                              n_by = 1,
                              n_draw = 3,
                              n_comp = 2,
                              indep = FALSE,
                              age_labels = NULL)
  expect_identical(nrow(ans$matrix), 2L * nrow(unique(ans$ans[c("age", "sexgender")])))
  expect_identical(ncol(ans$matrix), 4L)
})

test_that("'generate_ssvd_helper' works with valid inputs - indep = TRUE, n_by = 2", {
  set.seed(0)
  ans <- generate_ssvd_helper(ssvd = LFP,
                              n_along = 3,
                              n_by = 2,
                              n_draw = 3,
                              n_comp = 2,
                              indep = TRUE,
                              age_labels = NULL)
  expect_identical(nrow(ans$matrix), 3L * nrow(unique(ans$ans[c("by", "age", "sexgender")])))
  expect_identical(ncol(ans$matrix), 24L)
  expect_identical(nrow(ans$matrix_along_by), 3L)
})


## 'get_term_from_est' --------------------------------------------------------

test_that("'get_term_from_est' works", {
  est <- list(effectfree = c(a = 1, a = 2, b = 3, c = 4, c = 5),
              hyper = c(a = 1),
              hyperrandfree = double(),
              log_disp = c(disp = 3))
  ans_obtained <- get_term_from_est(est = est, index_term = c(1L, 4L, 7L))
  ans_expected <- c("a", "c", "disp")
  expect_identical(ans_obtained, ans_expected)
})


## 'get_disp' -----------------------------------------------------------------

test_that("'get_disp' works - unfitted", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 5)
  set.seed(1)
  ans_obtained <- get_disp(mod)
  set.seed(1)
  ans_expected <- draw_vals_disp(mod, n_sim = 5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_disp' works - fitted", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 5)
  mod <- fit(mod)
  set.seed(1)
  ans_obtained <- get_disp(mod)
  set.seed(1)
  ans_expected <- rvec::rvec(matrix(mod$draws_disp, nr = 1))
  expect_identical(ans_obtained, ans_expected)
})  


## 'insert_after' -------------------------------------------------------

test_that("'insert_after' works with data frames", {
  df <- data.frame(x = 1:3, y = 3:1)
  x <- 11:13
  nm_x = "new"
  ans_obtained <- insert_after(df = df,
                               nm_after = "x",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- data.frame(x = 1:3, new = 11:13, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
  ans_obtained <- insert_after(df = df,
                               nm_after = "y",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- data.frame(x = 1:3, y = 3:1, new = 11:13)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'insert_after' works with tibbles", {
  df <- tibble::tibble(x = 1:3, y = 3:1)
  x <- 11:13
  nm_x = "new"
  ans_obtained <- insert_after(df = df,
                               nm_after = "x",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- tibble(x = 1:3, new = 11:13, y = 3:1)
  expect_identical(ans_obtained, ans_expected)
  ans_obtained <- insert_after(df = df,
                               nm_after = "y",
                               x = x,
                               nm_x = nm_x)
  ans_expected <- tibble(x = 1:3, y = 3:1, new = 11:13)
  expect_identical(ans_obtained, ans_expected)
})


## 'is_same_class' ------------------------------------------------------------

test_that("'is_same_class' returns TRUE when classes same", {
    expect_true(is_same_class(AR1(), AR1()))
    expect_true(is_same_class(1L, 2L))
})

test_that("'is_same_class' returns FALSE when classes different", {
    expect_false(is_same_class(AR1(), N()))
    expect_false(is_same_class(1L, FALSE))
})


## 'make_combined_matrix_effect_outcome' -----------------------------------------

test_that("'make_combined_matrix_effect_outcome' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_matrix_effect_outcome(mod)
    expect_identical(nrow(ans_obtained), nrow(data))
    expect_identical(ncol(ans_obtained), length(make_terms_effects(mod$dimnames_terms)))
    expect_false(all(ans_obtained[,1] == 0))
})


## 'make_combined_matrix_effectfree_effect' -----------------------------------------

test_that("'make_combined_matrix_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_matrix_effectfree_effect(mod)
    expect_identical(nrow(ans_obtained), length(make_terms_effects(mod$dimnames_terms)))
    expect_identical(ncol(ans_obtained), length(make_terms_effectfree(mod)))
})


## 'make_combined_offset_effectfree_effect' -----------------------------------------

test_that("'make_combined_offset_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_combined_offset_effectfree_effect(mod)
    expect_identical(length(ans_obtained), length(make_terms_effects(mod$dimnames_terms)))
    expect_true(all(ans_obtained == 0))
})


## 'make_comp_components' -----------------------------------------------------

test_that("'make_comp_components' works - no hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_n_draw(mod, n = 1)
    mod <- fit(mod)
    draws <- make_draws_components(mod)
    ans <- make_comp_components(mod)
    expect_identical(length(draws), length(ans))
    expect_setequal(ans, c("effect", "hyper", "disp"))
})

test_that("'make_comp_components' works - has hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(sex:time ~ Lin())
    mod <- set_n_draw(mod, n = 1)
    mod <- fit(mod)
    draws <- make_draws_components(mod)
    ans <- make_comp_components(mod)
    expect_identical(length(draws), length(ans))
    expect_setequal(ans, c("effect", "hyper", "trend", "error", "disp"))
})

test_that("'make_comp_components' works - has svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD))
  mod <- set_prior(mod, age:sex ~ Sp())
  set.seed(0)
  mod <- fit(mod)
  term <- make_term_components(mod)
  ans <- make_comp_components(mod)
  expect_identical(length(term), length(ans))
  expect_setequal(ans, c("effect", "hyper", "svd", "spline", "disp"))
})


## 'make_comp_hyperrand' ------------------------------------------------------

test_that("'make_comp_hyperrand' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex:time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin())
  ans_obtained <- make_comp_hyperrand(mod)
  ans_expected <- c("hyper", "hyper", rep(c("trend", "error"), each = 12))
  expect_identical(ans_obtained, ans_expected)                      
})


## 'make_copies_repdata' ------------------------------------------------------

test_that("'make_copies_repdata' works with valid inputs", {
    data <- tibble::tibble(age = 0:4, deaths = 1:5, population = 11:15)
    n <- 2
    ans_obtained <- make_copies_repdata(data = data, n = n)
    ans_expected <- tibble::tibble(.replicate = factor(rep(c("Original", "Replicate 1", "Replicate 2"),
                                                           each = 5),
                                                       levels = c("Original",
                                                                  "Replicate 1", "Replicate 2")),
                                   age = rep(0:4, 3),
                                   deaths = rep(1:5, 3),
                                   population = rep(11:15, 3))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_components' -----------------------------------------------

test_that("'make_draws_components' works - no svd, spline", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  set.seed(0)
  ans_obtained <- make_draws_components(mod)
  expect_true(rvec::is_rvec(ans_obtained))
  expect_identical(length(ans_obtained),
                   length(make_effectfree(mod)) +
                     length(make_hyper(mod)) +
                     length(make_hyperrand(mod)) + 1L)
})

test_that("'make_draws_components' works - has spline", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp(n_comp = 6))
  mod <- fit(mod)
  set.seed(0)
  ans_obtained <- make_draws_components(mod)
  expect_true(rvec::is_rvec(ans_obtained))
  expect_identical(length(ans_obtained),
                   length(make_effectfree(mod)) + length(unique(data$age)) +
                     length(make_hyper(mod)) +
                     length(make_hyperrand(mod)) + 1L)
})

test_that("'make_draws_components' works - has svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD, n_comp = 3))
  mod <- fit(mod)
  set.seed(0)
  ans_obtained <- make_draws_components(mod)
  expect_true(rvec::is_rvec(ans_obtained))
  expect_identical(length(ans_obtained),
                   length(make_effectfree(mod)) + length(unique(data$age)) +
                     length(make_hyper(mod)) +
                     length(make_hyperrand(mod)) + 1L)
})

test_that("'make_draws_components' works - has hyperrand", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age:sex ~ Lin())
  mod <- fit(mod)
  set.seed(0)
  ans_obtained <- make_draws_components(mod)
  expect_true(rvec::is_rvec(ans_obtained))
})


## 'make_draws_disp' ----------------------------------------------------

test_that("'make_draws_disp' works", {
    set.seed(0)
    draws_post <- matrix(rnorm(13 * 5), nrow = 13)
    ans_obtained <- make_draws_disp(draws_post)
    ans_expected <- exp(draws_post[13,])
    expect_identical(unname(ans_obtained), ans_expected)
})


## 'make_draws_effectfree' ----------------------------------------------------

test_that("'make_draws_effectfree' works", {
  set.seed(0)
  est <- list(effectfree = rnorm(10),
              hyper = rnorm(2),
              hyperrand = numeric(),
              log_disp = 0.5)
  draws_post <- matrix(rnorm(13 * 5), nrow = 13)
  ans_obtained <- make_draws_effectfree(est = est, draws_post = draws_post)
  ans_expected <- draws_post[1:10,]
  expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_hyper' ----------------------------------------------------

test_that("'make_draws_hyper' works", {
  set.seed(0)
  est <- list(effectfree = rnorm(10),
              hyper = runif(4),
              hyperrand = numeric(),
              log_disp = 0.5)
  transforms_hyper <- list(identity, identity, exp, exp)
  draws_post <- matrix(rnorm(15 * 5), nrow = 15)
  ans_obtained <- make_draws_hyper(est = est,
                                   transforms_hyper = transforms_hyper,
                                   draws_post = draws_post)
  ans_expected <- draws_post[11:14, ]
  ans_expected[3:4,] <- exp(ans_expected[3:4,])
  expect_identical(unname(ans_obtained), ans_expected)
})


## 'make_draws_hyperrandfree' -------------------------------------------------

test_that("'make_draws_hyperrandfree' works - has hyperrandfree", {
  set.seed(0)
  est <- list(effectfree = rnorm(10),
              hyper = runif(4),
              hyperrandfree = rnorm(5),
              log_disp = 0.5)
  draws_post <- matrix(rnorm(20 * 5), nrow = 20)
  ans_obtained <- make_draws_hyperrandfree(est = est,
                                           draws_post = draws_post)
  ans_expected <- draws_post[15:19, ]
  expect_identical(unname(ans_obtained), ans_expected)
})

test_that("'make_draws_hyperrandfree' works - no hyperrandfree", {
  set.seed(0)
  est <- list(effectfree = rnorm(10),
              hyper = runif(4),
              hyperrandfree = numeric(),
              log_disp = 0.5)
  draws_post <- matrix(rnorm(15 * 5), nrow = 15)
  ans_obtained <- make_draws_hyperrandfree(est = est,
                                           draws_post = draws_post)
  ans_expected <- matrix(0, nrow = 0, ncol = 5)
  expect_identical(unname(ans_obtained), ans_expected)
})


## 'make_draws_post' ------------------------------------------------------

test_that("'make_draws_post' works with valid inputs - has R_prec", {
  set.seed(0)
  est <- list(effectfree = c("(Intercept)" = -3,
                             sex = c(-1, 1),
                             age = rnorm(10)),
              log_disp = 0.2)
  prec <- diag(12)
  prec <- Matrix::Matrix(prec)
  map <- list(effectfree = factor(c(1, NA, NA, 2:11)),
              disp = factor(1))
  ans <- make_draws_post(est = est,
                         prec = prec,
                         map = map,
                         n_draw = 500000L)
  expect_equal(rowMeans(ans), unlist(est, use.names = FALSE), tolerance = 0.02)
  expect_equal(solve(cov(t(ans[c(1, 4:14),]))), as.matrix(prec), tolerance = 0.02)
})


## 'fit_default' --------------------------------------------------------------

## lots more tests for 'fit' method

test_that("'fit_default' works with pois, optimzier is 'multi'", {
  set.seed(10)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(age ~ AR()) |>
    set_prior(age:sex ~ RW2(sd = 0)) |>
    set_prior(age:sex:time ~ AR())
  ans_obtained <- fit_default(mod, optimizer = "multi", quiet = TRUE, aggregate = TRUE,
                              start_oldpar = FALSE)
  expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit_default' works with pois, optimzier is 'nlminb'", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- fit_default(mod, optimizer = "nlminb", quiet = TRUE, aggregate = TRUE,
                              start_oldpar = FALSE)
  expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit_default' works with pois - start_oldpar", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit_default(mod, optimizer = "nlminb", quiet = TRUE, aggregate = TRUE,
                     start_oldpar = FALSE)
  ans_obtained <- fit_default(mod, optimizer = "nlminb", quiet = TRUE, aggregate = TRUE,
                              start_oldpar = TRUE)
  expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit_default' gives error with 'start_oldpar' if model fitted", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_error(fit_default(mod, optimizer = "nlminb", quiet = TRUE, aggregate = TRUE,
                           start_oldpar = TRUE),
               "`start_oldpar` is TRUE but model has not been fitted.")
})


## 'fit_inner_outer' ----------------------------------------------------------

test_that("'fit_inner_outer' works with with pois", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rnbinom(n = nrow(data), mu = 0.1 * data$popn, size = 100)
  formula <- deaths ~ age * sex + region * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  set.seed(0)
  ans_inner_outer <- fit_inner_outer(mod,
                                     optimizer = "nlminb",
                                     quiet = TRUE,
                                     start_oldpar = FALSE,
                                     vars_inner = c("age", "sex"))
  set.seed(0)
  ans_default <- fit_default(mod,
                             optimizer = "nlminb",
                             quiet = TRUE,
                             start_oldpar = FALSE,
                             aggregate = TRUE)
  aug_inner_outer <- ans_inner_outer |>
  augment()
  fit_inner_outer <- rvec::draws_median(aug_inner_outer$.fitted)
  aug_default <- ans_default |>
  augment()
  fit_default <- rvec::draws_median(aug_default$.fitted)
  expect_true(cor(fit_inner_outer, fit_default) > 0.98)
})

test_that("'fit_inner_outer' works with with binom", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), prob = 0.2, size = data$popn)
  formula <- deaths ~ age * sex + region * time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  set.seed(0)
  ans_inner_outer <- fit_inner_outer(mod,
                                     optimizer = "nlminb",
                                     quiet = TRUE,
                                     start_oldpar = FALSE,
                                     vars_inner = NULL)
  set.seed(0)
  ans_default <- fit_default(mod,
                             optimizer = "nlminb",
                             quiet = TRUE,
                             start_oldpar = FALSE,
                             aggregate = TRUE)
  aug_inner_outer <- ans_inner_outer |>
  augment()
  fit_inner_outer <- rvec::draws_median(aug_inner_outer$.fitted)
  aug_default <- ans_default |>
  augment()
  fit_default <- rvec::draws_median(aug_default$.fitted)
  expect_true(cor(fit_inner_outer, fit_default) > 0.98)
})

test_that("'fit_inner_outer' works with with norm", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$wt <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data), mean = data$age + data$time/100, sd = 5 / sqrt(data$wt))
  formula <- income ~ age * sex + region * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  set.seed(0)
  ans_inner_outer <- fit_inner_outer(mod,
                                     optimizer = "BFGS",
                                     quiet = TRUE,
                                     start_oldpar = FALSE,
                                     vars_inner = c("age", "sex"))
  set.seed(0)
  ans_default <- fit_default(mod,
                             optimizer = "BFGS",
                             quiet = TRUE,
                             start_oldpar = FALSE,
                             aggregate = TRUE)
  aug_inner_outer <- ans_inner_outer |>
    augment()
  fit_inner_outer <- rvec::draws_median(aug_inner_outer$.fitted)
  aug_default <- ans_default |>
    augment()
  fit_default <- rvec::draws_median(aug_default$.fitted)
  expect_true(cor(fit_inner_outer, fit_default) > 0.98)
})

test_that("'fit_inner_outer' throws error when 'start_oldpar' is TRUE", {
  set.seed(0)
  data <- expand.grid(age = 0:5,
                      time = 2000:2003,
                      sex = c("F", "M"),
                      region = c("a", "b"))
  data$wt <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data), mean = data$age + data$time/100, sd = 5 / sqrt(data$wt))
  formula <- income ~ age * sex + region * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  set.seed(0)
  expect_error(fit_inner_outer(mod,
                               optimizer = "BFGS",
                               quiet = TRUE,
                               start_oldpar = TRUE,
                               vars_inner = c("age", "sex")),
               "`start_oldpar` must be FALSE when using \"inner-outer\" method.")
})



## 'make_along_mod' -----------------------------------------------------------

test_that("'make_along_mod' works", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:time ~ Sp(n_comp = 5, along = "age"))
  ans_obtained <- make_along_mod(mod)
  ans_expected <- c("(Intercept)" = NA,
                    sex = NA,
                    age = "age",
                    time = "time",
                    "sex:age" = "age",
                    "age:time" = "age")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_hyperrand' -----------------------------------------------------------

test_that("'make_hyperrand' works - has hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(sex:time ~ Lin())
    mod <- set_n_draw(mod, n = 1)
    mod <- fit(mod)
    ans <- make_hyperrand(mod)
    expect_identical(length(ans), 26L)
})

test_that("'make_hyperrand' works - no hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
                    set_prior(sex:time ~ AR())
    mod <- set_n_draw(mod, n = 1)
    mod <- fit(mod)
    ans <- make_hyperrand(mod)
    expect_identical(length(ans), 0L)
})


## 'make_hyperrand_lin' -------------------------------------------------------

test_that("'make_hyperrand_lin' works with main effect", {
  prior <- Lin()
  hyperrandfree <- rvec::rnorm_rvec(n = 1, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_lin(prior = prior,
                                     hyperrandfree = hyperrandfree,
                                     effectfree = effectfree,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  trend <- hyperrandfree * (1:10 - mean(1:10))
  error <- effectfree - trend
  slope <- trend[2] - trend[1]
  ans_expected <- c(slope, trend, error)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_lin' works with interaction, con is 'none'", {
  prior <- Lin_AR()
  hyperrandfree <- rvec::rnorm_rvec(n = 2, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 20, n_draw = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_lin(prior = prior,
                                     hyperrandfree = hyperrandfree,
                                     effectfree = effectfree,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  trend <- c(hyperrandfree[1] * (1:10 - mean(1:10)),
             hyperrandfree[2] * (1:10 - mean(1:10)))
  error <- effectfree - trend
  slope <- c(trend[2] - trend[1], trend[12] - trend[11])
  ans_expected <- c(slope, trend, error)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_lin' works with interaction, con is 'by'", {
  prior <- Lin_AR(con = "by")
  hyperrandfree <- rvec::rnorm_rvec(n = 1, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_lin(prior = prior,
                                     hyperrandfree = hyperrandfree,
                                     effectfree = effectfree,
                                     dimnames_term = dimnames_term,
                                     var_time = var_time,
                                     var_age = var_age,
                                     var_sexgender = var_sexgender)
  trend <- c(-sqrt(0.5) * hyperrandfree * (1:10 - mean(1:10)),
             sqrt(0.5) * hyperrandfree * (1:10 - mean(1:10)))
  error <- c(-sqrt(0.5) * effectfree, sqrt(0.5) * effectfree) - trend
  slope <- c(trend[2] - trend[1], trend[12] - trend[11])
  ans_expected <- c(slope, trend, error)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_hyperrand_randomseasfix' -----------------------------------------------

test_that("'make_hyperrand_randomseasfix' works with main effect", {
  prior <- RW_Seas(n = 4)
  hyperrandfree <- rvec::rnorm_rvec(n = 3, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_randomseasfix(prior = prior,
                                               hyperrandfree = hyperrandfree,
                                               effectfree = effectfree,
                                               dimnames_term = dimnames_term,
                                               var_time = var_time,
                                               var_age = var_age,
                                               var_sexgender = var_sexgender)
  season <- c(hyperrandfree[1],
              hyperrandfree[2],
              hyperrandfree[3],
              -sum(hyperrandfree))[c(1:4, 1:4, 1:2)]
  trend <- effectfree - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_randomseasfix' works with interaction, random_sum is FALSE", {
  set.seed(0)
  prior <- RW2_Seas(n = 4)
  hyperrandfree <- rvec::rnorm_rvec(n = 6, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 20, n_draw = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_randomseasfix(prior = prior,
                                             hyperrandfree = hyperrandfree,
                                             effectfree = effectfree,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             var_sexgender = var_sexgender)
  season1 <- c(hyperrandfree[1],
               hyperrandfree[2],
               hyperrandfree[3],
               -sum(hyperrandfree[1:3]))[c(1:4, 1:4, 1:2)]
  season2 <- c(hyperrandfree[4],
               hyperrandfree[5],
               hyperrandfree[6],
               -sum(hyperrandfree[4:6]))[c(1:4, 1:4, 1:2)]
  season <- c(season1, season2)
  trend <- effectfree - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_randomseasfix' works with interaction, con is 'by'", {
  prior <- RW_Seas(n = 4, con = "by")
  hyperrandfree <- rvec::rnorm_rvec(n = 3, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_randomseasfix(prior = prior,
                                             hyperrandfree = hyperrandfree,
                                             effectfree = effectfree,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             var_sexgender = var_sexgender)
  season <- vctrs::vec_c(hyperrandfree[1],
                         hyperrandfree[2],
                         hyperrandfree[3],
                         -sum(hyperrandfree[1:3]))[c(1:4, 1:4, 1:2)]
  season <- c(-sqrt(0.5) * season,
              sqrt(0.5) * season)
  trend <- c(-sqrt(0.5) * effectfree, sqrt(0.5) * effectfree) - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_hyperrand_randomseasvary' ----------------------------------------------

test_that("'make_hyperrand_randomseasvary' works with main effect", {
  prior <- RW_Seas(n = 4, s_seas = 1)
  hyperrandfree <- rvec::rnorm_rvec(n = 8, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_randomseasvary(prior = prior,
                                              hyperrandfree = hyperrandfree,
                                              effectfree = effectfree,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  season <- c(hyperrandfree[1:3],
              -sum(hyperrandfree[1:3]),
              hyperrandfree[4:6],
              -sum(hyperrandfree[4:6]),
              hyperrandfree[7:8])
  trend <- effectfree - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_randomseasvary' works with interaction, con is 'none'", {
  set.seed(0)
  prior <- RW2_Seas(n = 4, s_seas = 1)
  hyperrandfree <- rvec::rnorm_rvec(n = 16, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 20, n_draw = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_randomseasvary(prior = prior,
                                              hyperrandfree = hyperrandfree,
                                              effectfree = effectfree,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  season1 <- c(hyperrandfree[1:3],
               -sum(hyperrandfree[1:3]),
               hyperrandfree[4:6],
               -sum(hyperrandfree[4:6]),
               hyperrandfree[7:8])
  season2 <- c(hyperrandfree[9:11],
               -sum(hyperrandfree[9:11]),
               hyperrandfree[12:14],
               -sum(hyperrandfree[12:14]),
               hyperrandfree[15:16])
  season <- c(season1, season2)
  trend <- effectfree - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_randomseasvary' works with interaction, con is 'by'", {
  prior <- RW_Seas(n_seas = 4, s_seas = 1, con = "by")
  hyperrandfree <- rvec::rnorm_rvec(n = 8, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_randomseasvary(prior = prior,
                                              hyperrandfree = hyperrandfree,
                                              effectfree = effectfree,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  season <- vctrs::vec_c(hyperrandfree[1:3],
                         -sum(hyperrandfree[1:3]),
                         hyperrandfree[4:6],
                         -sum(hyperrandfree[4:6]),
                         hyperrandfree[7:8])
  season <- c(-sqrt(0.5) * season,
              sqrt(0.5) * season)
  trend <- c(-sqrt(0.5) * effectfree, sqrt(0.5) * effectfree) - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_hyperrand_zeroseasfix' -----------------------------------------------

test_that("'make_hyperrand_zeroseasfix' works with main effect", {
  prior <- RW_Seas(n = 4, sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 2, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_zeroseasfix(prior = prior,
                                             hyperrandfree = hyperrandfree,
                                             effectfree = effectfree,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             var_sexgender = var_sexgender)
  season <- c(effectfree[1],
              effectfree[1] + hyperrandfree[1],
              effectfree[1] + hyperrandfree[2],
              -3 * effectfree[1] - sum(hyperrandfree))[c(1:4, 1:4, 1:2)]
  trend <- effectfree - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_zeroseasfix' works with interaction, con is 'none'", {
  set.seed(0)
  prior <- RW2_Seas(n = 4, sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 4, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 20, n_draw = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_zeroseasfix(prior = prior,
                                             hyperrandfree = hyperrandfree,
                                             effectfree = effectfree,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             var_sexgender = var_sexgender)
  season1 <- c(effectfree[1],
               effectfree[1] + hyperrandfree[1],
               effectfree[1] + hyperrandfree[2],
               -3 * effectfree[1] - sum(hyperrandfree[1:2]))[c(1:4, 1:4, 1:2)]
  season2 <- c(effectfree[11],
               effectfree[11] + hyperrandfree[3],
               effectfree[11] + hyperrandfree[4],
               -3 * effectfree[11] - sum(hyperrandfree[3:4]))[c(1:4, 1:4, 1:2)]
  season <- c(season1, season2)
  trend <- effectfree - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_zeroseasfix' works with interaction, con is 'by'", {
  prior <- RW_Seas(n = 4, sd = 0, con = "by")
  hyperrandfree <- rvec::rnorm_rvec(n = 2, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_zeroseasfix(prior = prior,
                                             hyperrandfree = hyperrandfree,
                                             effectfree = effectfree,
                                             dimnames_term = dimnames_term,
                                             var_time = var_time,
                                             var_age = var_age,
                                             var_sexgender = var_sexgender)
  season <- vctrs::vec_c(effectfree[1],
                         effectfree[1] + hyperrandfree[1],
                         effectfree[1] + hyperrandfree[2],
                         -3 * effectfree[1] - sum(hyperrandfree[1:2]))[c(1:4, 1:4, 1:2)]
  season <- c(-sqrt(0.5) * season,
              sqrt(0.5) * season)
  trend <- c(-sqrt(0.5) * effectfree, sqrt(0.5) * effectfree) - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_hyperrand_zeroseasvary' ----------------------------------------------

test_that("'make_hyperrand_zeroseasvary' works with main effect", {
  prior <- RW_Seas(n = 4, s_seas = 1, sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 7, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10, n_draw = 10)
  dimnames_term <- list(time = 2001:2010)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_zeroseasvary(prior = prior,
                                              hyperrandfree = hyperrandfree,
                                              effectfree = effectfree,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  season <- c(effectfree[1],
              effectfree[1] + hyperrandfree[1],
              effectfree[1] + hyperrandfree[2],
              -3 * effectfree[1] - sum(hyperrandfree[1:2]),
              effectfree[1] + hyperrandfree[3:5],
              -3 * effectfree[1] - sum(hyperrandfree[3:5]),
              effectfree[1] + hyperrandfree[6:7])
  trend <- effectfree - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_zeroseasvary' works with interaction, con is 'none'", {
  set.seed(0)
  prior <- RW2_Seas(n = 4, s_seas = 1, sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 14, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 20, n_draw = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_zeroseasvary(prior = prior,
                                              hyperrandfree = hyperrandfree,
                                              effectfree = effectfree,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  season1 <- c(effectfree[1],
               effectfree[1] + hyperrandfree[1],
               effectfree[1] + hyperrandfree[2],
               -3 * effectfree[1] - sum(hyperrandfree[1:2]),
               effectfree[1] + hyperrandfree[3:5],
               -3 * effectfree[1] - sum(hyperrandfree[3:5]),
               effectfree[1] + hyperrandfree[6:7])
  season2 <- c(effectfree[11],
               effectfree[11] + hyperrandfree[8],
               effectfree[11] + hyperrandfree[9],
               -3 * effectfree[11] - sum(hyperrandfree[8:9]),
               effectfree[11] + hyperrandfree[10:12],
               -3 * effectfree[11] - sum(hyperrandfree[10:12]),
               effectfree[11] + hyperrandfree[13:14])
  season <- c(season1, season2)
  trend <- effectfree - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_hyperrand_zeroseasvary' works with interaction, con is 'by'", {
  prior <- RW_Seas(n_seas = 4, s_seas = 1, con = "by", sd = 0)
  hyperrandfree <- rvec::rnorm_rvec(n = 7, n_draw = 10)
  effectfree <- rvec::rnorm_rvec(n = 10)
  dimnames_term <- list(time = 2001:2010, sex = c("f", "m"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_hyperrand_zeroseasvary(prior = prior,
                                              hyperrandfree = hyperrandfree,
                                              effectfree = effectfree,
                                              dimnames_term = dimnames_term,
                                              var_time = var_time,
                                              var_age = var_age,
                                              var_sexgender = var_sexgender)
  season <- vctrs::vec_c(effectfree[1],
                         effectfree[1] + hyperrandfree[1],
                         effectfree[1] + hyperrandfree[2],
                         -3 * effectfree[1] - sum(hyperrandfree[1:2]),
                         effectfree[1] + hyperrandfree[3:5],
                         -3 * effectfree[1] - sum(hyperrandfree[3:5]),
                         effectfree[1] + hyperrandfree[6:7])
  season <- c(-sqrt(0.5) * season,
              sqrt(0.5) * season)
  trend <- c(-sqrt(0.5) * effectfree, sqrt(0.5) * effectfree) - season
  ans_expected <- c(trend, season)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_levels_spline' -------------------------------------------------------

test_that("'make_levels_spline' works - unlist is FALSE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp(n_comp = 5))
  mod <- set_prior(mod, age:sex ~ Sp(n_comp = 5))
  mod <- set_prior(mod, age:time ~ Sp(n_comp = 5, along = "age"))
  set.seed(0)
  ans_obtained <- make_levels_spline(mod, unlist = FALSE)
  ans_expected <- list("(Intercept)" = NULL,
                       sex = NULL,
                       age = paste0("comp", 1:5),
                       time = NULL,
                       "sex:age" = paste(c("F", "M"), rep(paste0("comp", 1:5), each = 2), sep = "."),
                       "age:time" = paste0("comp", 1:5, ".", rep(2000:2005, each = 5)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_spline' works - unlist is TRUE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp(n_comp = 5))
  mod <- set_prior(mod, age:sex ~ Sp(n_comp = 5))
  mod <- set_prior(mod, age:time ~ Sp(n_comp = 5, along = "age"))
  set.seed(0)
  ans_obtained <- make_levels_spline(mod, unlist = TRUE)
  ans_expected <- c(paste0("comp", 1:5),
                    paste(c("F", "M"), rep(paste0("comp", 1:5), each = 2), sep = "."),
                    paste(paste0("comp", 1:5), rep(2000:2005, each = 5), sep = "."))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_spline_term' --------------------------------------------------

test_that("'make_levels_spline_term' works - con is 'none'", {
  prior <- Sp(n = 5)
  dimnames_term <- list(reg = 1:2,
                        age = 1:20)
  ans_obtained <- make_levels_spline_term(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = "time",
                                          var_age = "age")
  ans_expected <- paste(1:2, rep(paste0("comp", 1:5), each = 2), sep = ".")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_spline_term' works - con is 'by'", {
  prior <- Sp(n = 5, con = "by")
  dimnames_term <- list(reg = 1:2,
                        age = 1:20)
  ans_obtained <- make_levels_spline_term(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = "time",
                                          var_age = "age")
  ans_expected <- paste("reg1", paste0("comp", 1:5), sep = ".")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_spline_term' works - con is 'by', more dimensions", {
  prior <- Sp(n = 5, con = "by")
  dimnames_term <- list(reg = 1:4,
                        age = 1:20,
                        sex = c("f", "m"))
  ans_obtained <- make_levels_spline_term(prior = prior,
                                          dimnames_term = dimnames_term,
                                          var_time = "time",
                                          var_age = "age")
  ans_expected <- paste(paste0("reg", 1:3),
                        rep(paste0("comp", 1:5), each = 3),
                        rep("sex1", times = 15),
                        sep = ".")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_svd' ----------------------------------------------------------

test_that("'make_levels_svd' works - unlist is FALSE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD))
  mod <- set_prior(mod, age:sex ~ SVD(HMD))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD))
  set.seed(0)
  ans_obtained <- make_levels_svd(mod, unlist = FALSE)
  ans_expected <- list("(Intercept)" = NULL,
                       sex = NULL,
                       age = paste0("comp", 1:3),
                       time = NULL,
                       "sex:age" = paste0(rep(c("F", "M"), each = 3), ".comp", 1:3),
                       "age:time" = paste0("comp", 1:3, ".", rep(2000:2005, each = 3)))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_svd' works - unlist is TRUE", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * age + age*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD, n_comp = 5))
  mod <- set_prior(mod, age:sex ~ SVD(HMD, n_comp = 5))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD, n_comp = 5))
  set.seed(0)
  ans_obtained <- make_levels_svd(mod, unlist = TRUE)
  ans_expected <- c(paste0("comp", 1:5),
                    paste0(rep(c("F", "M"), each = 5), ".comp", 1:5),
                    paste0("comp", 1:5, ".", rep(2000:2005, each = 5)))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_svd_term' ----------------------------------------------------------

test_that("'make_levels_svd_term' works - total, main effect", {
  prior <- SVD(HMD)
  dimnames_term <- list(age = c(0:59, "60+"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- paste0("comp", 1:3)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_svd_term' works - joint, age:sex", {
  prior <- SVD(HMD, n_comp = 5)
  dimnames_term <- list(age = c(0:59, "60+"), sex = c("M", "F"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- paste(rep(c("M", "F"), each = 5), paste0("comp", 1:5), sep = ".")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_svd_term' works - indep, age:sex:reg", {
  prior <- SVD(HMD, indep = FALSE, n_comp = 5)
  dimnames_term <- list(reg = 1:2, age = c(0:59, "60+"), sex = c("M", "F"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- paste(paste0("comp", 1:5),
                        rep(1:2, each = 5),
                        sep = ".")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_svd_term' works - indep, age:sex:time, con is 'none'", {
  prior <- SVD_RW(HMD, indep = FALSE, n_comp = 5)
  dimnames_term <- list(time = 2001:2010, age = c(0:59, "60+"), sex = c("M", "F"))
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- paste(paste0("comp", 1:5),
                        rep(2001:2010, each = 5),
                        sep = ".")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_svd_term' works - indep, age:time:reg, con is 'by'", {
  prior <- SVD_RW(HMD, indep = FALSE, n_comp = 5, con = "by")
  dimnames_term <- list(time = 2001:2010, age = c(0:59, "60+"), reg = c("a", "b", "c"))
  var_age <- "age"
  var_sexgender <- "sex"
  var_time <- "time"
  ans_obtained <- make_levels_svd_term(prior = prior,
                                       dimnames_term = dimnames_term,
                                       var_time = var_time,
                                       var_age = var_age,
                                       var_sexgender = var_sexgender)
  ans_expected <- paste(paste0("comp", 1:5),
                        rep(2001:2010, each = 5),
                        rep(c("reg1", "reg2"), each = 50),
                        sep = ".")
  expect_identical(ans_obtained, ans_expected)
})




## 'make_lin_trend' -----------------------------------------------------------

test_that("'make_lin_trend' works with valid inputs - n_by = 1", {
  slope <- rvec::rvec(matrix(as.numeric(1:5), nr = 1))
  matrix_along_by <- matrix(0:9, nr = 10)
  ans_obtained <- make_lin_trend(slope = slope,
                                 matrix_along_by = matrix_along_by)
  ones <- rep(1, times = 10)
  s <- 1:10
  ans_expected <- rvec::rvec(outer(ones, -0.5 * 11 * (1:5)) + outer(s, 1:5))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_lin_trend' works with valid inputs - n_by = 2, transposed", {
  slope <- rvec::rvec(matrix(rnorm(10), nr = 2))
  intercept <- -0.5 * 6 * slope
  matrix_along_by <- t(matrix(0:9, nr = 2))
  ans_obtained <- make_lin_trend(slope = slope,
                                 matrix_along_by = matrix_along_by)
  s <- 1:5
  ans_expected <- c(intercept[1] + slope[1] * s, intercept[2] + slope[2] * s)
  ans_expected <- ans_expected[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10)]
  expect_identical(ans_obtained, ans_expected)
})


## 'make_stored_draws' --------------------------------------------------------

test_that("'make_stored_draws' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex ~ Known(c(0.1, -0.1)))
  mod <- set_n_draw(mod, n = 10)
  est <- list(effectfree = c(rnorm(11), 0.1, -0.1),
              hyper = rnorm(1),
              hyperrandfree = numeric(),
              disp = runif(1))
  prec <- crossprod(matrix(rnorm(169), nr = 13))
  map <- make_map(mod)
  ans <- make_stored_draws(mod = mod,
                           est = est,
                           prec = prec,
                           map = map)
  expect_identical(ncol(ans$draws_effectfree), 10L)
  expect_identical(ncol(ans$draws_hyper), 10L)
  expect_identical(length(ans$draws_disp), 10L)
  ans2 <- make_stored_draws(mod = mod,
                            est = est,
                            prec = prec,
                            map = map)
  expect_identical(ans, ans2)
})


## 'make_stored_point' --------------------------------------------------------

test_that("'make_stored_draws' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex ~ Known(c(0.1, -0.1)))
  est <- list(effectfree = c(rnorm(11), 0.1, -0.1),
              hyper = rnorm(1),
              hyperrandfree = numeric(),
              log_disp = runif(1))
  ans <- make_stored_point(mod = mod,
                           est = est)
  expect_identical(ans$point_effectfree, est$effectfree)
  expect_identical(ans$point_hyper, exp(est$hyper))
  expect_identical(ans$point_hyperrandfree, double())
  expect_identical(ans$point_disp, exp(est$log_disp))
})


## 'make_par_disp' ------------------------------------------------------------

test_that("'make_par_disp' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + time + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_n_draw(mod, n = 10)
    mod <- fit(mod)
    components <- components(mod)
    meanpar <- exp(make_linpred_comp(components = components,
                                     data = mod$data,
                                     dimnames_terms = mod$dimnames_terms))
    disp <- components$.fitted[components$component == "disp"]
    set.seed(1)
    ans_obtained <- make_par_disp(mod,
                                  meanpar = meanpar,
                                  disp = disp)
    set.seed(1)
    ans_expected <- rvec::rgamma_rvec(n = length(meanpar),
                                      data$deaths + 1/disp,
                                      data$popn + 1/(disp*meanpar))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_par_disp' works with bage_mod_binom", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
  formula <- deaths ~ age + time + sex
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  mod <- fit(mod)
  mod <- set_n_draw(mod, n = 10)
  components <- components(mod)
  invlogit <- function(x) 1 / (1 + exp(-x))
  meanpar <- invlogit(make_linpred_comp(components = components,
                                        data = mod$data,
                                        dimnames_terms = mod$dimnames_terms))
  disp <- components$.fitted[components$component == "disp"]
  set.seed(1)
  ans_obtained <- make_par_disp(mod,
                                meanpar = meanpar,
                                disp = disp)
  set.seed(1)
  ans_expected <- rvec::rbeta_rvec(n = length(meanpar),
                                   data$deaths + meanpar/disp,
                                   data$popn - data$deaths + (1 - meanpar)/disp)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_is_fixed' ------------------------------------------------------------

test_that("'make_is_fixed' works when nothing fixed", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    est <- list(effectfree = make_effectfree(mod),
                hyper =  make_hyper(mod),
                hyperrand = make_hyperrand(mod),
                log_disp = 0)
    map <- make_map(mod)
    expect_true(is.null(map))
    ans_obtained <- make_is_fixed(est = est, map = map)
    ans_expected <- rep(FALSE, times = length(unlist(est)))
})

test_that("'make_is_fixed' works when Known prior", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex ~ Known(c(0.1, -0.1)))
    mod <- fit(mod)
    est <- list(effectfree = make_effectfree(mod),
                hyper =  make_hyper(mod),
                hyperrand = make_hyperrand(mod),
                log_disp = 0)
    map <- make_map(mod)
    ans_obtained <- make_is_fixed(est = est, map = map)
    ans_expected <- rep(c(FALSE, TRUE, FALSE),
                        times = c(11,
                                  2,
                                  20 + 6 + length(est$hyper) +
                                    + length(est$log_disp)))
    expect_identical(unname(ans_obtained), ans_expected)
})


## 'make_level_components' ----------------------------------------------------

test_that("'make_level_components' works - no hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_level_components(mod)
    expect_identical(length(ans), length(comp))
})

test_that("'make_level_components' works - has hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod <- set_prior(mod, sex:time ~ Lin())
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_level_components(mod)
    expect_identical(length(ans), length(comp))
})


## 'make_levels_hyper' --------------------------------------------------------

test_that("'make_levels_hyper' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_levels_hyper(mod)
    ans_expected <- c(age = "sd",
                      time = "sd",
                      "age:sex" = "sd")
    expect_identical(ans_obtained, ans_expected)                      
})


## 'make_levels_hyperrand' ----------------------------------------------------

test_that("'make_levels_hyperrand' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex:time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin())
  ans_obtained <- make_levels_hyperrand(mod, unlist = TRUE)
  ans_expected <- c("slope.F", "slope.M",
                    rep(paste(c("F", "M"), rep(2000:2005, each = 2), sep = "."), 2))
  expect_identical(ans_obtained, ans_expected)                      
  ans_obtained <- make_levels_hyperrand(mod, unlist = FALSE)
  ans_expected <- list(character(),
                       character(),
                       c("slope.F", "slope.M",
                         rep(paste(c("F", "M"), rep(2000:2005, each = 2), sep = "."), 2)))
  expect_identical(ans_obtained, ans_expected)                      
})


## 'make_levels_replicate' ----------------------------------------------------

test_that("'make_levels_replicate' works", {
    ans_obtained <- make_levels_replicate(n = 2, n_row_data = 3)
    ans_expected <- factor(rep(c("Original", "Replicate 1", "Replicate 2"),
                               each = 3),
                           levels = c("Original", "Replicate 1", "Replicate 2"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_linpred_comp' --------------------------------------------------------

test_that("'make_linpred_comp' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 10L)
  comp <- components(mod, quiet = TRUE)
  ans <- make_linpred_comp(components = comp,
                           data = mod$data,
                           dimnames_terms = mod$dimnames_terms)
  expect_identical(length(ans), length(mod$outcome))
})


## 'make_linpred_raw' ---------------------------------------------------------

test_that("'make_linpred_raw' works with valid inputs - point is FALSE", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 10L)
  mod <- fit(mod)
  ans_obtained <- make_linpred_raw(mod, point = FALSE)
  comp <- components(mod, quiet = TRUE)
  ans_expected <- make_linpred_comp(components = comp,
                                    data = mod$data,
                                    dimnames_terms = mod$dimnames_terms)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_linpred_raw' works with valid inputs - point is TRUE", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n_draw = 10L)
  mod <- fit(mod)
  ans_obtained <- make_linpred_raw(mod, point = TRUE)
  m1 <- make_combined_matrix_effect_outcome(mod)
  m2 <- make_combined_matrix_effectfree_effect(mod)
  ans_expected <- as.double(m1 %*% m2 %*% mod$point_effectfree)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_point_est_effects' ---------------------------------------------------

test_that("'make_point_est_effects' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(age ~ RW(sd = 0)) |>
    set_prior(age:sex ~ RW(sd = 0))
  mod <- fit(mod)
  ans_obtained <- make_point_est_effects(mod)
  int <- unname(mod$point_effectfree[1])
  age <- c(0, unname(mod$point_effectfree[2:10]))
  sex <- unname(mod$point_effectfree[11:12])
  agesex <- c(0, unname(mod$point_effectfree[13:21]),
              0, unname(mod$point_effectfree[22:30]))
  ans_expected <- list("(Intercept)" = int,
                       age = age,
                       sex = sex,
                       "age:sex" = agesex)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_point_est_effects' throws correct error when not fitted", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_error(make_point_est_effects(mod),
               "Internal error: Model not fitted.")
})


## 'make_scaled_eigen' --------------------------------------------------------

## See also tests for rvnorm_eigen

test_that("'make_scaled_eigen' works with positive definite matrix", {
    set.seed(0)
    prec <- solve(crossprod(matrix(rnorm(25), 5)))
    ans <- make_scaled_eigen(prec)
    expect_identical(dim(ans), dim(prec))
})

test_that("'make_scaled_eigen' works with non-negative definite matrix", {
    set.seed(0)
    prec <- solve(crossprod(matrix(rnorm(25), 5)))
    prec[5,] <- 0
    prec[,5] <- 0
    ans <- make_scaled_eigen(prec)
    expect_identical(dim(ans), dim(prec))
})


## 'make_spline' --------------------------------------------------------------

test_that("'make_spline' works", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age ~ RW(sd = 0))
  mod <- set_prior(mod, age:sex ~ Sp(n = 5))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  effectfree <- mod$draws_effectfree
  ans_obtained <- make_spline(mod = mod, effectfree = effectfree)
  ans_expected <- effectfree[23:32,]
  expect_equal(ans_obtained, ans_expected)
})


## 'make_svd' -----------------------------------------------------------------

test_that("'make_svd' works - SVD_RW, random", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ SVD(HMD, n_comp = 5))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD, n_comp = 5))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  effectfree <- mod$draws_effectfree
  ans_obtained <- make_svd(mod = mod, effectfree = effectfree)
  ans_expected <- effectfree[24:63,]
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_svd' works - SVD_RW, zero", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ SVD(HMD, n_comp = 5))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD, n_comp = 5, sd = 0))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  effectfree <- mod$draws_effectfree
  ans_obtained <- make_svd(mod = mod, effectfree = effectfree)
  ans_expected <- rbind(effectfree[24:33,],
                        matrix(0, nrow = 5, ncol = 5),
                        effectfree[34:58,])
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_svd' works - SVD_AR", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 20)
  formula <- deaths ~ age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:time ~ SVD_AR1(HMD, n_comp = 1))
  mod <- set_n_draw(mod, n = 5)
  mod <- fit(mod)
  effectfree <- mod$draws_effectfree
  ans_obtained <- make_svd(mod = mod, effectfree = effectfree)
  ans_expected <- effectfree[22:27,]
  expect_equal(ans_obtained, ans_expected)
})


## 'make_term_components' -----------------------------------------------------

test_that("'make_term_components' works - no disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod <- set_prior(mod, age ~ Sp())
    mod <- set_n_draw(mod, n = 1)       
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_term_components(mod)
    expect_identical(length(ans), length(comp))
})

test_that("'make_term_components' works - has hyperrand", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ Lin())
    mod <- set_n_draw(mod, n = 1)       
    mod <- fit(mod)
    comp <- make_comp_components(mod)
    ans <- make_term_components(mod)
    expect_identical(length(ans), length(comp))
})

test_that("'make_term_components' works - has svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 1)       
  mod <- fit(mod)
  comp <- make_comp_components(mod)
  ans <- make_term_components(mod)
  expect_identical(length(ans), length(comp))
})


## 'make_term_spline' ------------------------------------------------------------

test_that("'make_term_spline' works - no spline", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  ans_obtained <- make_term_spline(mod)
  ans_expected <- factor()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_term_spline' works - has spline", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ Sp(n_comp = 5))
  ans_obtained <- make_term_spline(mod)
  ans_expected <- factor(rep("age", times = 5))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_term_svd' ------------------------------------------------------------

test_that("'make_term_svd' works - no svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 3)
  ans_obtained <- make_term_svd(mod)
  ans_expected <- factor()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_term_svd' works - has svd", {
  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt", max = 60),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 5)
  mod <- set_prior(mod, age ~ SVD(HMD))
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD))
  ans_obtained <- make_term_svd(mod)
  ans_expected <- factor(c(rep("age", times = 3),
                           rep("age:time", times = 3 * 6)))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_hyperrand' ----------------------------------------------------

test_that("'make_terms_hyperrand' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex:time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin())
  ans_obtained <- make_terms_hyperrand(mod)
  ans_expected <- factor(rep("sex:time", 26),
                         levels = c("(Intercept)", "age", "sex:time"))
  expect_identical(ans_obtained, ans_expected)                      
})


## 'make_unconstr_dimnames_by' ------------------------------------------------

test_that("'make_unconstr_dimnames_by' works", {
  dimnames_term <- list(age = 1:5, time = 1:6, reg = 1:3)
  ans_obtained <- make_unconstr_dimnames_by(i_along = 2L,
                                            dimnames_term = dimnames_term)
  ans_expected <- list(age = paste0("age", 1:4), reg = paste0("reg", 1:2))
  expect_identical(ans_obtained, ans_expected)
})


## 'paste_dot' ----------------------------------------------------------------

test_that("'paste_dot' works with valid inputs", {
  expect_identical(paste_dot(1:3, 3:1), c("1.3", "2.2", "3.1"))
})


## 'rescale_lin_intercept' ----------------------------------------------------

test_that("'rescale_lin_intercept' works with valid inputs", {
  set.seed(0)
  slope <- rvec::rnorm_rvec(n = 4, n_draw = 10)
  effect <- rvec::rnorm_rvec(n = 20, n_draw = 10)
  matrix_along_by <- matrix(0:19, nrow = 5)
  ans_obtained <- rescale_lin_intercept(slope = slope,
                                        effect = effect,
                                        matrix_along_by = matrix_along_by)
  ans_expected <- slope
  for (i in 1:4)
    ans_expected[i] <- mean(effect[1:5 + (i - 1) * 5]) - mean(slope[i] * (1:5))
  expect_equal(ans_obtained, ans_expected)
})


## 'make_transforms_hyper' ----------------------------------------------------

test_that("'make_transforms_hyper' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ans_obtained <- make_transforms_hyper(mod)
    ans_expected <- list(exp, exp)
    expect_identical(unname(ans_obtained), ans_expected,
                     ignore_function_env = TRUE)
})


## 'infer_trend_cyc_seas_err_forecast' --------------------------------------------

test_that("'infer_trend_cyc_seas_err_forecast' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(time ~ RW2()) |>
    set_prior(sex:time ~ Lin()) |>
    fit()
  mod <- set_n_draw(mod, 5)
  mod <- fit(mod)
  comp_est <- components(mod)
  comp_forecast <- forecast(mod, labels = 2006:2007, output = "components")
  dimnames_terms_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                          var_time = mod$var_time,
                                                          labels_forecast = 2006:2007,
                                                          time_only = TRUE)
  ans_obtained <- infer_trend_cyc_seas_err_forecast(components = comp_forecast,
                                                    priors = mod$priors,
                                                    dimnames_terms = dimnames_terms_forecast,
                                                    var_time = mod$var_time,
                                                    var_age = mod$var_age)
  expect_equal(ans_obtained[1:3], comp_forecast[1:3])
})


## 'infer_trend_cyc_seas_err_seasfix_forecast' --------------------------------

test_that("'infer_trend_cyc_seas_err_seasfix_forecast' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n = 3)) |>
                  set_n_draw(n = 10) |>
                  fit()
  components <- components(mod)
  ans <- infer_trend_cyc_seas_err_seasfix_forecast(prior = mod$priors[["sex:time"]],
                                                   dimnames_term = mod$dimnames_terms[["sex:time"]],
                                                   var_time = mod$var_time,
                                                   var_age = mod$var_age,
                                                   components = components)
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  expect_equal(effect, season + trend)
})


## 'infer_trend_cyc_seas_err_seasvary_forecast' -------------------------------

test_that("'infer_trend_cyc_seas_err_seasvary_forecast' works", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ sex * time + age
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_prior(sex:time ~ RW_Seas(n = 3, s = 1)) |>
                  set_n_draw(n = 10) |>
                  fit()
  components <- components(mod)
  ans <- infer_trend_cyc_seas_err_seasvary_forecast(prior = mod$priors[["sex:time"]],
                                                    dimnames_term = mod$dimnames_terms[["sex:time"]],
                                                    var_time = mod$var_time,
                                                    var_age = mod$var_age,
                                                    components = components)
  season <- ans$.fitted[ans$term == "sex:time" & ans$component == "season"]
  trend <- ans$.fitted[ans$term == "sex:time" & ans$component == "trend"]
  effect <- ans$.fitted[ans$term == "sex:time" & ans$component == "effect"]
  expect_equal(effect, season + trend)
})


## 'rvec_to_mean' -------------------------------------------------------------

test_that("'rvec_to_mean' works with valid inputs", {
  data <- tibble::tibble(a = 1:3,
                         b = rvec::rvec(matrix(1:12, nr = 3)),
                         c = rvec::rvec(matrix(FALSE, nr = 3, nc = 2)),
                         d = c("a", "b", "c"))
  ans_obtained <- rvec_to_mean(data)
  ans_expected <- tibble::tibble(a = 1:3,
                         b = rowMeans(matrix(1:12, nr = 3)),
                         c = rowMeans(matrix(FALSE, nr = 3, nc = 2)),
                         d = c("a", "b", "c"))
  expect_identical(ans_obtained, ans_expected)
})


## 'rmvnorm_chol', 'rmvnorm_eigen' --------------------------------------------

test_that("'rmvnorm_chol' and 'rmvnorm_eigen' give the same answer", {
    set.seed(0)
    prec <- crossprod(matrix(rnorm(25), 5))
    mean <- rnorm(5)
    R_prec <- chol(prec)
    scaled_eigen <- make_scaled_eigen(prec)
    ans_chol <- rmvnorm_chol(n = 100000, mean = mean, R_prec = R_prec)
    ans_eigen <- rmvnorm_eigen(n = 100000, mean = mean, scaled_eigen = scaled_eigen)
    expect_equal(rowMeans(ans_chol), rowMeans(ans_eigen), tolerance = 0.02)
    expect_equal(cov(t(ans_chol)), cov(t(ans_eigen)), tolerance = 0.02)
})


## 'sort_components' ----------------------------------------------------------

test_that("'sort_components' works with valid inputs", {
  components <- tibble::tribble(~term,         ~component, ~level,
                                "sex",          "effect",   "m",
                                "time",         "season", "2000",
                                "sex",          "hyper",    "sd",
                                "sex",          "effect",   "f",
                                "(Intercept)", "effect",   "(Intercept)",
                                "time",         "effect", "2000")
  mod <- list(formula = deaths ~ time + sex)
  ans_obtained <- sort_components(components = components, mod = mod)
  ans_expected <- tibble::tribble(~term,         ~component, ~level,
                                  "(Intercept)", "effect",   "(Intercept)",
                                  "time",         "effect",   "2000",
                                  "time",         "season",   "2000",
                                  "sex",          "effect",   "m",
                                  "sex",          "effect",   "f",
                                  "sex",          "hyper",    "sd")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'sort_components' raises correct effor with invalid component", {
  components <- tibble::tribble(~term,         ~component, ~level,
                                "(Intercept)", "effect",   "(Intercept)",
                                "time",         "season", "2000",
                                "sex",          "wrong",    "sd")
  expect_error(sort_components(components, mod = list(formula = deaths ~ age)),
               "Internal error: \"wrong\" not a valid value for `component`.")
})


## 'transform_hyper_ar' -------------------------------------------------------

test_that("'transform_hyper_ar' works with 'bage_prior_ar - AR1'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    0.18 * ans + 0.8
  }
  l <- transform_hyper_ar(prior = AR1())
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), exp(0.35))
})

test_that("'transform_hyper_ar' works with 'bage_prior_svd_ar - AR'", {
  shifted_invlogit <- function(x) {
    ans <- exp(x) / (1 + exp(x))
    2 * ans - 1
  }
  l <- transform_hyper_ar(prior = SVD_AR(HMD))
  expect_equal(l[[1]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[2]](0.35), shifted_invlogit(0.35))
  expect_equal(l[[3]](0.35), exp(0.35))
})
