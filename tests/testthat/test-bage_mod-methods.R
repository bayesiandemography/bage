
## 'augment' ---------------------------------------------------------------

test_that("'augment' works with Poisson, disp - has data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_fitted <- fit(mod)
    ans <- augment(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), c(".observed", ".fitted", ".expected")))
})

test_that("'augment' calculates fitted in cells with missing outcome or offset -  Poisson", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2010, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$popn[1] <- NA
    data$deaths[2] <- NA
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_unfit <- augment(mod, quiet = TRUE)
    mod_fitted <- fit(mod)
    ans_fit <- augment(mod_fitted)
    expect_false(any(rvec::draws_any(is.na(ans_fit$.fitted))))
    expect_true(".deaths" %in% names(ans_fit))
    expect_true(all(rvec::draws_all(ans_fit$.deaths[-2] == ans_fit$deaths[-2])))
    expect_equal(rvec::draws_mean(ans_fit$.deaths[2]),
                 rvec::draws_mean(ans_fit$.fitted[2] * ans_fit$popn[2]),
                 tolerance = 0.02)
    expect_false(".deaths" %in% names(ans_unfit))
})

test_that("'augment' works with Poisson, disp - no data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    aug_notfitted <- augment(mod, quiet = TRUE)
    mod_fitted <- fit(mod)
    aug_fitted <- augment(mod_fitted)
    expect_identical(names(aug_fitted), names(aug_notfitted))
})

test_that("'augment' works with binomial, no disp - has data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.5)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod_fitted <- fit(mod)
    ans <- augment(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), c(".observed", ".fitted")))
})

test_that("'augment' works with normal - with data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$deaths <- rpois(n = nrow(data), lambda = 100)
    data$deaths[1] <- NA
    formula <- deaths ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    mod_fitted <- fit(mod)
    ans <- augment(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), ".deaths", ".fitted"))
})

test_that("'augment' works with normal - no data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$deaths <- rpois(n = nrow(data), lambda = 100)
    formula <- deaths ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    ans <- augment(mod, quiet = TRUE)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), ".fitted"))
})

test_that("'augment' gives same answer when run twice - with data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.5)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_fitted <- fit(mod)
    ans1 <- augment(mod_fitted, quiet = TRUE)
    ans2 <- augment(mod_fitted, quiet = TRUE)
    expect_identical(ans1, ans2)
})

test_that("'augment' gives same answer when run twice - no data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.5)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans1 <- augment(mod, quiet = TRUE)
    ans2 <- augment(mod, quiet = TRUE)
    expect_identical(ans1, ans2)
})

test_that("'augment' gives message when used unfitted and quiet is FALSE", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_message(augment(mod),
                   "Model not fitted, so values drawn straight from prior distribution.")
})


## 'components' ---------------------------------------------------------------

test_that("'components' works with no disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod_fitted <- fit(mod)
    ans <- components(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(unique(ans$component), c("effect", "hyper"))
})

test_that("'components' works with no data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    comp_nodata <- components(mod, quiet = TRUE)
    mod_data <- fit(mod)
    comp_data <- components(mod_data)
    comp_merge <- merge(comp_nodata, comp_data, by = c("term", "component", "level"))
    expect_identical(nrow(comp_merge), nrow(comp_data))
})

test_that("'components' gives same answer when run twice - with data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod_fitted <- fit(mod)
    ans1 <- components(mod_fitted, quiet = TRUE)
    ans2 <- components(mod_fitted, quiet = TRUE)
    expect_identical(ans1, ans2)
})

test_that("'components' gives same answer when run twice - no data", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans1 <- components(mod, quiet = TRUE)
    ans2 <- components(mod, quiet = TRUE)
    expect_identical(ans1, ans2)
})

test_that("'components' gives message when used unfitted and quiet is FALSE", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_message(components(mod),
                   "Model not fitted, so values drawn straight from prior distribution.")
})

test_that("'disp' estimates not affected by weights in normal model", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$income <- rnorm(n = nrow(data), mean = 10, sd = 3)
    data$wt1 <- runif(n = nrow(data), max = 5)
    data$wt2 <- 20 * data$wt1 
    formula <- income ~ age + sex + time
    mod1 <- mod_norm(formula = formula,
                    data = data,
                    weights = wt1)
    mod1_fitted <- fit(mod1)
    comp1 <- components(mod1_fitted)
    disp1 <- comp1$.fitted[comp1$term == "disp"]
    mod2 <- mod_norm(formula = formula,
                    data = data,
                    weights = wt2)
    mod2_fitted <- fit(mod2)
    comp2 <- components(mod1_fitted)
    disp2 <- comp2$.fitted[comp1$term == "disp"]
    expect_equal(rvec::draws_mean(disp1), rvec::draws_mean(disp2))
})


## 'computations' -------------------------------------------------------------

test_that("'computations' returns NULL (with warning) if applied to unfitted model", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_message({x <- computations(mod)},
                   "Model not fitted..")
    expect_true(is.null(x))
})

test_that("'computations' returns tibble if applied to fitted model", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
      fit()
    expect_true(tibble::is_tibble(computations(mod)))
})


## 'draw_vals_augment_fitted' -------------------------------------------------

test_that("'draw_vals_augment_fitted' works with Poisson, has disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_fitted <- fit(mod)
    ans <- draw_vals_augment_fitted(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), c(".observed", ".fitted", ".expected")))
})

test_that("'draw_vals_augment_fitted' calculates fitted in cells with missing outcome or offset -  Poisson", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$popn[1] <- NA
    data$deaths[2] <- NA
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod_fitted <- fit(mod)
    ans <- draw_vals_augment_fitted(mod_fitted)
    expect_false(any(rvec::draws_any(is.na(ans$.fitted))))
})

test_that("'draw_vals_augment_fitted' works with binomial, no disp", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.5)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    mod_fitted <- fit(mod)
    ans <- draw_vals_augment_fitted(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), c(".observed", ".fitted")))
})

test_that("'draw_vals_augment_fitted' works with normal", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                        KEEP.OUT.ATTRS = FALSE)
    data$deaths <- rpois(n = nrow(data), lambda = 100)
    formula <- deaths ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    mod_fitted <- fit(mod)
    ans <- draw_vals_augment_fitted(mod_fitted)
    expect_true(is.data.frame(ans))
    expect_identical(names(ans),
                     c(names(data), ".fitted"))
})

test_that("'draw_vals_augment_fitted' works with Poisson, has rr3", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- poputils::rr3(rpois(n = nrow(data), lambda = 10))
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
                  set_datamod_outcome_rr3() |>
                  set_n_draw(5)
  mod_fitted <- fit(mod)
  ans <- draw_vals_augment_fitted(mod_fitted)
  expect_true(is.data.frame(ans))
  expect_identical(names(ans),
                   c(names(data), c(".deaths", ".observed", ".fitted", ".expected")))
  aug_nodisp <- mod |>
  set_disp(mean = 0) |>
  fit() |>
  augment()
  expect_identical(names(aug_nodisp),
                   c(names(data), c(".deaths", ".observed", ".fitted")))
})

test_that("'draw_vals_augment_fitted' works with binomial, has rr3", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- poputils::rr3(rbinom(n = nrow(data), size = data$popn, prob = 0.4))
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn) |>
                   set_datamod_outcome_rr3() |>
                   set_n_draw(5)
  mod_fitted <- fit(mod)
  ans <- draw_vals_augment_fitted(mod_fitted)
  expect_true(is.data.frame(ans))
  expect_identical(names(ans),
                   c(names(data), c(".deaths", ".observed", ".fitted", ".expected")))
  aug_nodisp <- mod |>
  set_disp(mean = 0) |>
  fit() |>
  augment()
  expect_identical(names(aug_nodisp),
                   c(names(data), c(".deaths", ".observed", ".fitted")))
})


## 'draw_vals_augment_unfitted' --------------------------------------------------------

test_that("'draw_vals_augment_unfitted' works with normal", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  formula <- deaths ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = 1)
  ans <- draw_vals_augment_unfitted(mod)
  expect_true(is.data.frame(ans))
  expect_identical(names(ans), c(names(data), ".fitted"))
})

test_that("'draw_vals_augment_unfitted' works with 'bage_mod_pois' - has disp", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 20)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  mod <- set_n_draw(mod, 10)
  ans_obtained <- draw_vals_augment_unfitted(mod)
  vals_components <- draw_vals_components_unfitted(mod, n_sim = 10)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_expected <- exp(make_linpred_comp(components = vals_components,
                                         data = mod$data,
                                         dimnames_terms = mod$dimnames_terms))
  set.seed(mod$seed_augment)
  vals_fitted <- draw_vals_fitted(mod = mod,
                                  vals_expected = vals_expected,
                                  vals_disp = vals_disp)
  vals_outcome <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "pois",
                                         outcome_obs = rep(NA_real_, times = nrow(data)),
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  ans_expected <- tibble::as_tibble(data)
  ans_expected$deaths <- vals_outcome
  ans_expected$.observed <- vals_outcome / mod$offset
  ans_expected$.fitted <- vals_fitted
  ans_expected$.expected <- vals_expected
  expect_equal(ans_obtained, ans_expected)
  expect_identical(names(augment(fit(mod))), names(ans_obtained))
})

test_that("'draw_vals_augment_unfitted' works with 'bage_mod_pois' - no disp", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 20)
  data$popn <- rpois(n = nrow(data), lambda = 30)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_disp(mod, mean = 0)
  mod <- set_n_draw(mod, 10)
  ans_obtained <- draw_vals_augment_unfitted(mod)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_fitted <- exp(make_linpred_comp(components = vals_components,
                                       data = mod$data,
                                       dimnames_term = mod$dimnames_terms))
  set.seed(mod$seed_augment)
  vals_outcome <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "pois",
                                         outcome_obs = rep(NA_real_, times = nrow(data)),
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  ans_expected <- tibble::as_tibble(data)
  ans_expected$deaths <- vals_outcome
  ans_expected$.observed <- vals_outcome / data$popn
  ans_expected$.fitted <- vals_fitted
  expect_equal(ans_obtained, ans_expected)
  expect_identical(names(augment(fit(mod))), names(ans_obtained))
})

test_that("'draw_vals_augment_unfitted' works with 'bage_mod_norm'", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$income <- rnorm(n = nrow(data), mean = 20, sd = 3)
  data$wt <- rpois(n = nrow(data), lambda = 100)
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  mod <- set_n_draw(mod, 10)
  set.seed(1)
  ans_obtained <- draw_vals_augment_unfitted(mod)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_disp <- vals_components$.fitted[vals_components$component == "disp"]
  vals_disp <- mod$outcome_sd * vals_disp
  scale_outcome <- get_fun_scale_outcome(mod)
  vals_fitted <- scale_outcome(make_linpred_comp(components = vals_components,
                                                 data = mod$data,
                                                 dimnames_term = mod$dimnames_terms))
  set.seed(mod$seed_augment)
  vals_outcome <- draw_vals_outcome_true(datamod = NULL,
                                         nm_distn = "norm",
                                         outcome_obs = rep(NA_real_, times = nrow(data)),
                                         fitted = vals_fitted,
                                         disp = vals_disp,
                                         offset = mod$offset)
  ans_expected <- tibble::as_tibble(mod$data)
  ans_expected$income <- vals_outcome
  ans_expected$.fitted <- vals_fitted
  expect_equal(ans_obtained, ans_expected)
  expect_identical(names(augment(fit(mod))), names(ans_obtained))
})


## 'draw_vals_fitted' ---------------------------------------------------------

test_that("'draw_vals_fitted' works with 'bage_mod_pois'", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  vals_disp <- draw_vals_disp(mod, n_sim = n_sim)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  vals_expected <- exp(make_linpred_comp(components = vals_components,
                                           data = mod$data,
                                           dimnames_term = mod$dimnames_terms))
  set.seed(1)
  ans_obtained <- draw_vals_fitted(mod,
                                   vals_expected = vals_expected,
                                   vals_disp = vals_disp)
  set.seed(1)
  ans_expected <- rvec::rgamma_rvec(n = nrow(data),
                                    shape = 1 / vals_disp,
                                    rate = 1 / (vals_disp * vals_expected))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'draw_vals_fitted' works with 'bage_mod_binom'", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  vals_disp <- draw_vals_disp(mod, n_sim = n_sim)
  vals_components <- draw_vals_components_unfitted(mod = mod,
                                                   n_sim = n_sim)
  invlogit <- function(x) exp(x) / (1 + exp(x))
  vals_expected <- invlogit(make_linpred_comp(components = vals_components,
                                              data = mod$data,
                                              dimnames_term = mod$dimnames_terms))
  set.seed(1)
  ans_obtained <- draw_vals_fitted(mod,
                                   vals_expected = vals_expected,
                                   vals_disp = vals_disp)
  set.seed(1)
  ans_expected <- rvec::rbeta_rvec(n = nrow(data),
                                   shape1 = vals_expected / vals_disp,
                                   shape2 = (1 - vals_expected) / vals_disp)
  expect_equal(ans_obtained, ans_expected)
})


## 'fit' -----------------------------------------------------------------
 
test_that("'fit' works with valid inputs - pois has exposure", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with valid inputs - pois has exposure", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$deaths <- rpois(n = nrow(data), lambda = 10000)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = 1)
    mod <- set_prior(mod, age ~ RW2(sd = 0))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with valid inputs - binom", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + sex + time
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with valid inputs - binom, disp is 0", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + sex + time
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn) |>
                    set_disp(mean = 0)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with valid inputs - norm", {
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data[] <- lapply(data, factor)
    data$wt <- rpois(n = nrow(data), lambda = 100)
    data$val <- rnorm(n = nrow(data), mean = (data$sex == "F"))
    formula <- val ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = wt)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with known intercept and sex effect", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, `(Intercept)` ~ Known(values = -2))
    mod <- set_prior(mod, sex ~ Known(values = c(-0.1, 0.1)))
    ans_obtained <- fit(mod)
    expect_equal(ans_obtained$draws_effectfree[1,1], -2)
    expect_equal(ans_obtained$draws_effectfree[12:13,1], c(-0.1, 0.1))
})

test_that("'fit' works with AR1", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + age:time + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ AR1())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' gives the same imputed rate when outcome is NA and offset is NA", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2002, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + age:time + time
    ## outcome NA
    set.seed(0)
    data_outcome <- data
    data_outcome$deaths[5] <- NA
    mod_outcome <- mod_pois(formula = formula,
                            data = data_outcome,
                            exposure = popn)
    mod_outcome <- fit(mod_outcome)
    ans_outcome <- augment(mod_outcome)$.fitted[5]
    ## offset NA
    set.seed(0)
    data_offset <- data
    data_offset$popn[5] <- NA
    mod_offset <- mod_pois(formula = formula,
                           data = data_offset,
                           exposure = popn)
    mod_offset <- fit(mod_offset)
    ans_offset <- augment(mod_offset)$.fitted[5]
    ## compare
    expect_equal(ans_outcome, ans_offset)
})

test_that("'fit' works when all observed values for one year are NA", {
    data <- data.frame(deaths = c(NA, 2:10),
                       age = rep(1:2, each = 5),
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ age + time,
                    data = data,
                    exposure = 1)
    mod_fitted <- fit(mod)
    expect_true(is_fitted(mod_fitted))
})

test_that("'fit' works when model consists of intercept only", {
    data <- data.frame(deaths = 1:10,
                       age = rep(1:2, each = 5),
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ 1,
                    data = data,
                    exposure = 1)
    mod_fitted <- fit(mod)
    expect_true(is_fitted(mod_fitted))
})

test_that("'fit' works when model has no hyper-parameters", {
    data <- data.frame(deaths = 1:10,
                       age = rep(1:2, each = 5),
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ 1,
                    data = data,
                    exposure = 1) |>
                    set_disp(mean = 0)
    mod_fitted <- fit(mod)
    expect_true(is_fitted(mod_fitted))
})

test_that("'fit' works when single dimension", {
    data <- data.frame(deaths = 1:10,
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ time,
                    data = data,
                    exposure = 1) |>
      set_prior(time ~ RW(sd = 0))
    mod_fitted <- fit(mod)
    expect_identical(nrow(mod_fitted$draws_effectfree), nrow(data))
})

test_that("'fit' works with SVD", {
    set.seed(0)
    data <- expand.grid(age = poputils::age_labels(type = "five", max = 60),
                        time = 2000:2005,
                        sex = c("Female", "Male"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + age:time + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age:sex ~ SVD(HMD))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with Lin", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex + age + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ Lin())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with Lin, n_by > 1", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex * time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ Lin())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with RW - n_by > 0", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex * time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ RW())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with RW2, n_by > 1", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2019, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex * time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ RW2())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with no hyper", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with AR", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ sex * time + age
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ AR(n = 2))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with SVD, n_by > 1", {
    set.seed(0)
    data <- expand.grid(age = c(0:59, "60+"), time = 2000:2005, reg = c("a", "b"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:reg + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age:reg ~ SVD(HMD))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with SVD, n_by > 1", {
    set.seed(0)
    data <- expand.grid(age = c(0:59, "60+"), time = 2000:2001, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex:time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age:sex:time ~ SVD(HMD))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with Lin_AR", {
    set.seed(0)
    data <- expand.grid(age = c(0:59, "60+"), time = 2000:2005, reg = c("a", "b"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:reg + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ Lin_AR(s = 0.2))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with Lin(s = 0)", {
    set.seed(0)
    data <- expand.grid(age = c(0:59, "60+"), time = 2000:2005, reg = c("a", "b"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:reg + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, time ~ Lin(s = 0))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works with RW2_Infant()", {
    set.seed(0)
    data <- expand.grid(age = c(0:59, "60+"), time = 2000:2005, reg = c("a", "b"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:reg + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, age:reg ~ RW2_Infant())
    ans_obtained <- fit(mod)
    mod <- set_prior(mod, age:reg ~ RW2_Infant(con = "by"))
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' and 'forecast' work with SVD_AR", {
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      time = 2001:2010)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- rpois(n = nrow(data), lambda = 0.05 * data$population)
  data$deaths[1] <- NA
  mod <- mod_pois(deaths ~ age:time,
                  data = data,
                  exposure = population) |>
                  set_prior(age:time ~ SVD_AR1(LFP))
  mod <- suppressWarnings(fit(mod))
  expect_true(is_fitted(mod))
  f <- forecast(mod, labels = 2011:2012)
  expect_setequal(c(names(f), ".deaths"),
                  names(augment(mod)))
})

test_that("'fit' and 'forecast' work with SVD_RW", {
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      time = 2001:2010)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- rpois(n = nrow(data), lambda = 0.05 * data$population)
  data$deaths[1] <- NA
  mod <- mod_pois(deaths ~ age:time,
                  data = data,
                  exposure = population) |>
                  set_prior(age:time ~ SVD_RW(LFP))
  mod <- fit(mod)
  expect_true(is_fitted(mod))
  f <- forecast(mod, labels = 2011:2012)
  expect_setequal(c(names(f), ".deaths"),
                  names(augment(mod)))
  mod <- mod |>
    set_prior(age:time ~ SVD_RW(LFP, sd = 0))
  mod <- fit(mod)
  expect_true(is_fitted(mod))
})

test_that("'fit' and 'forecast' work with SVD_RW2", {
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      time = 2001:2010)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- rpois(n = nrow(data), lambda = 0.05 * data$population)
  data$deaths[1] <- NA
  mod <- mod_pois(deaths ~ age:time,
                  data = data,
                  exposure = population) |>
                  set_prior(age:time ~ SVD_RW2(LFP))
  mod <- fit(mod)
  expect_true(is_fitted(mod))
  f <- forecast(mod, labels = 2011:2012)
  expect_setequal(c(names(f), ".deaths"),
                  names(augment(mod)))
  mod <- mod |>
    set_prior(age:time ~ SVD_RW2(LFP, sd = 0))
  mod <- fit(mod)
  expect_true(is_fitted(mod))
})

test_that("'fit' works inner-outer", {
  set.seed(0)
  ## structure of data
  data <- expand.grid(age = poputils::age_labels(type = "lt"),
                      sex = c("Female", "Male"),
                      time = 2011:2015,
                      region = 1:10)
  data$population <- runif(n = nrow(data), min = 10, max = 1000)
  data$deaths <- NA
  ## generate single dataset
  mod_sim <- mod_pois(deaths ~ age * sex + region + time,
                      data = data,
                      exposure = population) |>
                      set_prior(`(Intercept)` ~ NFix(s = 0.1)) |>
                      set_prior(age ~ RW(s = 0.02)) |>
                      set_prior(sex ~ NFix(sd = 0.1)) |>
                      set_prior(age:sex ~ SVD(HMD)) |>
                      set_prior(time ~ Lin_AR1(s = 0.05, sd = 0.02)) |>
                      set_prior(region ~ NFix(sd = 0.05)) |>
                      set_disp(mean = 0.005)
  data_sim <- mod_sim |>
  set_n_draw(n = 1) |>
  augment(quiet = TRUE) |>
  dplyr::mutate(deaths = rvec::draws_median(deaths)) |>
  dplyr::select(age, sex, time, region, population, deaths)
  ## specify estimation model
  mod_est <- mod_pois(deaths ~ age * sex + region + time,
                      data = data_sim,
                      exposure = population) |>
                      set_prior(age:sex ~ SVD(HMD)) |>
                      set_prior(time ~ Lin_AR())
  ## fit estimation model
  mod_est <- mod_est |>
  fit(method = "inner-outer")
  expect_true(is_fitted(mod_est))
})

test_that("'fit' works when optimizer is 'optim'", {
    set.seed(0)
    data <- expand.grid(age = c(0:59, "60+"), time = 2000:2005, reg = c("a", "b"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:reg + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works when 'quiet' is FALSE and optimizer is 'nlminb'", {
  set.seed(0)
  data <- expand.grid(age = c(0:59, "60+"), time = 2000:2005, reg = c("a", "b"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age:reg + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  suppressMessages(
    capture.output(ans_obtained <- fit(mod, quiet = FALSE), file = NULL)
  )
  expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works when 'quiet' is FALSE and optimizer is 'BFGS'", {
  set.seed(0)
  data <- expand.grid(age = c(0:59, "60+"), time = 2000:2005, reg = c("a", "b"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age:reg + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  suppressMessages(
    capture.output(ans_obtained <- fit(mod, optimizer = "BFGS", quiet = FALSE), file = NULL)
  )
  expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' works when 'quiet' is FALSE and optimizer is 'CG'", {
  set.seed(0)
  data <- expand.grid(age = c(0:59, "60+"), time = 2000:2005, reg = c("a", "b"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age:reg + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  suppressMessages(
    capture.output(ans_obtained <- fit(mod, optimizer = "CG", quiet = FALSE), file = NULL)
  )
  expect_s3_class(ans_obtained, "bage_mod")
})


## 'forecast' -----------------------------------------------------------------

test_that("'forecast' works with fitted model - output is 'augment'", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2004, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ans_no_est <- forecast(mod, labels = 2005:2006)
    ans_est <- forecast(mod, labels = 2005:2006, include_estimates = TRUE)
    expect_identical(names(ans_est), names(ans_no_est))
})

test_that("'forecast' works with fitted model - uses newdata", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2004, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    newdata <- make_data_forecast_labels(mod, labels = 2005)
    newdata$deaths <- rpois(n = nrow(newdata), lambda = 10)
    ans_est <- forecast(mod, newdata = newdata, include_estimates = TRUE)
    expect_identical(names(ans_est), names(augment(mod)))
})


test_that("'forecast' gives same answer when run twice - output is 'augment'", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2004, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_n_draw(mod, n = 10)
    mod <- fit(mod)
    ans1 <- forecast(mod, labels = 2005:2006)
    ans2 <- forecast(mod, labels = 2005:2006)
    ans3 <- forecast(mod, labels = 2005:2006,
                     output = "aug",
                     include_estimates = TRUE)
    ans3 <- ans3[ans3$time >= 2005,]
    expect_identical(ans1, ans2)
    expect_identical(ans1, ans3)
})

test_that("'forecast' works with fitted model - output is 'components'", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2004, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit(mod)
  ans_no_est <- forecast(mod,
                         labels = 2005:2006,
                         output = "comp")
  ans_est <- forecast(mod,
                      labels = 2005:2006,
                      output = "comp",
                      include_estimates = TRUE)
  expect_identical(names(ans_est), names(ans_no_est))
})

test_that("'forecast' gives same answer when run twice - output is 'components'", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2004, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  ans1 <- forecast(mod,
                   labels = 2005:2006,
                   output = "comp")
  ans2 <- forecast(mod,
                   labels = 2005:2006,
                   output = "components")
  expect_identical(ans1, ans2)
})

test_that("'forecast' gives same answer when run twice - output is 'components'", {
  set.seed(0)
  data <- expand.grid(age = 0:4, time = 2000:2004, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  ans1 <- forecast(mod,
                   labels = 2005:2006,
                   output = "comp")
  ans2 <- forecast(mod,
                   labels = 2005:2006,
                   output = "components")
  expect_identical(ans1, ans2)
})

test_that("'forecast' throws correct error when time var not identified'", {
    set.seed(0)
    data <- expand.grid(age = 0:4, epoch = 2000:2004, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    expect_error(forecast(mod,
                          labels = 2005:2006,
                          output = "comp"),
                 "Can't forecast when time variable not identified.")
})

test_that("'forecast' throws correct error when labels and newdata both supplied'", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2004, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    labels <- 2007:2008
    newdata <- make_data_forecast_labels(mod = mod, labels = labels)
    expect_error(forecast(mod,
                          labels = labels,
                          newdata = newdata),
                 "Values supplied for `newdata` and for `labels`.")
})

test_that("'forecast' throws error when neither labels nor newdata supplied", {
    set.seed(0)
    data <- expand.grid(age = 0:4, time = 2000:2004, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + sex * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    newdata <- make_data_forecast_labels(mod, 2005:2006)
    newdata$deaths <- rpois(n = nrow(newdata), lambda = 10)
    expect_error(forecast(mod, include_estimates = TRUE),
                 "No value supplied for `newdata` or for `labels`.")
})


## 'forecast_augment' --------------------------------------------------------

test_that("'forecast_augment' works - Poisson, has disp, no forecasted offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- rpois(n = nrow(data), lambda = 1000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- make_data_forecast_labels(mod = mod, labels_forecast = labels_forecast)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
})


test_that("'forecast_augment' works - Poisson, no offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  data_forecast <- make_data_forecast_labels(mod = mod, labels_forecast = 2006:2008)
  labels_forecast <- 2006:2008
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
  expect_true(rvec::is_rvec(ans$deaths))
  expect_true(all(is.na(ans$.observed)))
})

test_that("'forecast_augment' works - Poisson, has disp, has forecasted offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$exposure <- rpois(n = nrow(data), lambda = 1000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  data_forecast <- make_data_forecast_labels(mod = mod, labels_forecast = 2006:2008)
  labels_forecast <- 2006:2008
  data_forecast$exposure <- rpois(n = nrow(data_forecast), lambda = 1000)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
  expect_true(rvec::is_rvec(ans$deaths))
  expect_true(all(is.na(ans$.observed)))
})

test_that("'forecast_augment' works - Poisson, has disp, has forecasted offset, imputed historical est", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 100)
  data$deaths[1] <- NA
  data$exposure <- rpois(n = nrow(data), lambda = 1000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  data_forecast <- make_data_forecast_labels(mod = mod, labels_forecast = 2006:2008)
  labels_forecast <- 2006:2008
  data_forecast$exposure <- rpois(n = nrow(data_forecast), lambda = 1000)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
  expect_true(all(is.na(ans$deaths)))
  expect_true(all(is.na(ans$.observed)))
  expect_true(rvec::is_rvec(ans$.deaths))
})

test_that("'forecast_augment' works - Poisson, has disp, has forecasted offset, datamod", {
  set.seed(0)
  data <- expand.grid(age = 0:5, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 30) * 3
  data$deaths[1] <- NA
  data$exposure <- rpois(n = nrow(data), lambda = 1000)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = exposure)
  mod <- set_n_draw(mod, n = 10)
  mod <- set_datamod_outcome_rr3(mod)
  mod <- fit(mod)
  components_est <- components(mod)
  data_forecast <- make_data_forecast_labels(mod = mod, labels_forecast = 2006:2008)
  labels_forecast <- 2006:2008
  data_forecast$exposure <- rpois(n = nrow(data_forecast), lambda = 1000)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
  expect_true(rvec::is_rvec(ans$deaths))
  expect_true(all(is.na(ans$.observed)))
  expect_true(rvec::is_rvec(ans$.deaths))
  expect_true(all(rvec::extract_draw(ans$deaths) %% 3 == 0))
})

test_that("'forecast_augment' works - binomial, no disp", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  mod <- set_n_draw(mod, n = 10)
  mod <- set_disp(mod, mean = 0)
  mod <- fit(mod)
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- make_data_forecast_labels(mod= mod, labels_forecast = labels_forecast)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
})

test_that("'forecast_augment' works - normal", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age + sex * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = popn)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- make_data_forecast_labels(mod= mod, labels_forecast = labels_forecast)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
})

test_that("'forecast_augment' works - normal, has forecasted offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age + sex * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = popn)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- make_data_forecast_labels(mod= mod, labels_forecast = labels_forecast)
  data_forecast$popn <- rpois(n = nrow(data_forecast), lambda = 1000)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
  expect_true(rvec::is_rvec(ans$income))
})

test_that("'forecast_augment' works - normal, no offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age + sex * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = 1)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- make_data_forecast_labels(mod= mod, labels_forecast = labels_forecast)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
  expect_true(rvec::is_rvec(ans$income))
})


test_that("'forecast_augment' works - normal, estimated has imputed", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  data$income[1] <- NA
  formula <- income ~ age + sex * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = popn)
  mod <- set_n_draw(mod, n = 10)
  mod <- fit(mod)
  components_est <- components(mod)
  labels_forecast <- 2006:2008
  data_forecast <- make_data_forecast_labels(mod= mod, labels_forecast = labels_forecast)
  data_forecast$popn <- rpois(n = nrow(data_forecast), lambda = 1000)
  set.seed(1)
  components_forecast <- forecast_components(mod = mod,
                                             components_est = components_est,
                                             labels_forecast = labels_forecast)
  components <- vctrs::vec_rbind(components_est, components_forecast)
  dimnames_forecast <- make_dimnames_terms_forecast(dimnames_terms = mod$dimnames_terms,
                                                    var_time = mod$var_time,
                                                    labels_forecast = labels_forecast,
                                                    time_only = FALSE)
  linpred_forecast <- make_linpred_comp(components = components,
                                        data = data_forecast,
                                        dimnames_terms = dimnames_forecast)
  ans <- forecast_augment(mod = mod,
                          data_forecast = data_forecast,
                          linpred_forecast = linpred_forecast)
  aug_est <- augment(mod)
  expect_setequal(ans$age, aug_est$age)
  expect_setequal(ans$sex, aug_est$sex)
  expect_setequal(ans$time, 2006:2008)
  expect_identical(names(ans), names(aug_est))
  expect_true(rvec::is_rvec(ans$.income))
  expect_true(all(is.na(ans$income)))
})


## 'get_fun_ag_offset' --------------------------------------------------------

test_that("'get_fun_ag_offset' works with binom", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  f <- get_fun_ag_offset(mod)
  ans_obtained <- f(mod$offset)
  ans_expected <- sum(mod$offset)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_fun_ag_offset' works with norm", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$wt <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  f <- get_fun_ag_offset(mod)
  ans_obtained <- f(mod$offset)
  n <- length(mod$offset)
  ans_expected <- (n^2) / sum(1 / mod$offset)
  expect_identical(ans_obtained, ans_expected)
})


## 'get_fun_ag_outcome' --------------------------------------------------------

test_that("'get_fun_ag_outcome' works with binom", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
  formula <- deaths ~ age + sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  f <- get_fun_ag_outcome(mod)
  ans_obtained <- f(mod$outcome)
  ans_expected <- sum(mod$outcome)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_fun_ag_outcome' works with norm", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$wt <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age + sex + time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  f <- get_fun_ag_outcome(mod)
  ans_obtained <- f(mod$outcome)
  ans_expected <- mean(mod$outcome)
  expect_identical(ans_obtained, ans_expected)
})

## 'get_fun_inv_transform' ----------------------------------------------------

test_that("'get_fun_inv_transform' works with valid inputs", {
    set.seed(0)
    x <- runif(100)
    logit <- function(x) log(x) - log(1 - x)
    expect_equal(get_fun_inv_transform(structure(1, class = "bage_mod_pois"))(log(x)), x)
    expect_equal(get_fun_inv_transform(structure(1, class = "bage_mod_binom"))(logit(x)), x)
    expect_equal(get_fun_inv_transform(structure(1, class = "bage_mod_norm"))(x), x)
})


## 'get_fun_scale_outcome' --------------------------------------------------------

test_that("'get_fun_scale_outcome' works with valid inputs", {
    expect_equal(get_fun_scale_outcome(structure(1, class = c("bage_mod_pois", "bage_mod")))(1), 1)
    expect_equal(get_fun_scale_outcome(structure(1, class = c("bage_mod_binom", "bage_mod")))(1), 1)
    expect_equal(get_fun_scale_outcome(structure(list(outcome_mean = 3, outcome_sd = 2),
                                                 class = c("bage_mod_norm", "bage_mod")))(1), 5)
})


## 'get_nm_outcome' -----------------------------------------------------------

test_that("'get_nm_outcome' works with 'bage_mod_pois'", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- rpois(n = nrow(data), lambda = 20)
  data$popn <- rpois(n = nrow(data), lambda = 30)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_identical(get_nm_outcome(mod), "deaths")
})


## 'get_nm_outcome_obs' -------------------------------------------------------

test_that("'get_nm_outcome_obs' works with 'bage_mod_pois'", {
  set.seed(0)
  n_sim <- 10
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$deaths <- 3 * rpois(n = nrow(data), lambda = 10)
  data$popn <- rpois(n = nrow(data), lambda = 30)
  formula <- deaths ~ age + sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_identical(get_nm_outcome_obs(mod), "deaths")
  mod <- set_datamod_outcome_rr3(mod)
  expect_identical(get_nm_outcome_obs(mod), ".deaths")
})


## 'has_disp' -----------------------------------------------------------------

test_that("'has_disp' works with valid inputs", {
    data <- data.frame(deaths = 1:10,
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ time,
                    data = data,
                    exposure = 1)
    expect_true(has_disp(mod))
    mod <- set_disp(mod, mean = 0)
    expect_false(has_disp(mod))
})


## 'is_fitted' ----------------------------------------------------------------

test_that("'is_fitted' works with valid inputs", {
  data <- data.frame(deaths = 1:10,
                     time = 2001:2010)
  mod <- mod_pois(deaths ~ time,
                  data = data,
                  exposure = 1)
  expect_false(is_fitted(mod))
  mod <- fit(mod)
  expect_true(is_fitted(mod))
})


## 'make_i_lik_mod' -----------------------------------------------------------

test_that("'make_i_lik_mod' works with bage_mod_pois", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- 3 * rpois(n = nrow(data), lambda = 5)
  formula <- deaths ~ age + time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_identical(make_i_lik_mod(mod), 303L)
  mod0 <- set_disp(mod, mean = 0)
  expect_identical(make_i_lik_mod(mod0), 301L)
  expect_identical(make_i_lik_mod(set_datamod_outcome_rr3(mod)), 304L)
  expect_identical(make_i_lik_mod(set_datamod_outcome_rr3(mod0)), 302L)
})

test_that("'make_i_lik_mod' works with bage_mod_binom", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- 3 * rbinom(n = nrow(data), size = data$popn, prob = 0.02)
  formula <- deaths ~ age + time + sex
  mod <- mod_binom(formula = formula,
                  data = data,
                  size = popn)
  expect_identical(make_i_lik_mod(mod), 103L)
  mod0 <- set_disp(mod, mean = 0)
  expect_identical(make_i_lik_mod(mod0), 101L)
  expect_identical(make_i_lik_mod(set_datamod_outcome_rr3(mod)), 104L)
  expect_identical(make_i_lik_mod(set_datamod_outcome_rr3(mod0)), 102L)
})

test_that("'make_i_lik_mod' works with bage_mod_norm", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$wt <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data), sd = 10)
  formula <- income ~ age + time + sex
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  expect_identical(make_i_lik_mod(mod), 201L)
  mod$datamod_outcome <- "wrong"
  expect_error(make_i_lik_mod(mod),
               "Internal error: Invalid inputs.")
})


## 'make_mod_disp' -----------------------------------------------------------

test_that("'make_mod_disp' works with pois", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rpois(n = nrow(data), lambda = 0.3 * data$popn)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit(mod)
  mod_disp <- make_mod_disp(mod)
  expect_setequal(names(mod_disp$priors), "(Intercept)")
  mu <- exp(make_linpred_raw(mod, point = TRUE))
  expect_equal(mod_disp$offset, mod$offset * mu)
  expect_true(mod_disp$mean_disp > 0)
  expect_identical(length(mod_disp$dimnames_terms), 1L)
})

test_that("'make_mod_disp' works with pois - large dataset", {
  set.seed(0)
  data <- expand.grid(time = 1:6000, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rpois(n = nrow(data), lambda = 0.3 * data$popn)
  formula <- deaths ~ sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_prior(time ~ RW(sd = 0))
  mod$point_effectfree <- rnorm(1 + 5999 + 2)
  mod_disp <- make_mod_disp(mod)
  expect_true(identical(nrow(mod_disp$data), 10000L))
})

test_that("'make_mod_disp' works with binom", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), size =  data$popn, prob = 0.3)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  mod <- fit(mod)
  mod_disp <- make_mod_disp(mod)
  expect_setequal(names(mod_disp$priors), names(mod$priors))
  expect_true(mod_disp$mean_disp > 0)
  expect_s3_class(mod_disp$priors[["age"]], "bage_prior_known")
  expect_equal(mod_disp$offset, mod$offset)
})

test_that("'make_mod_disp' works with binom", {
  set.seed(0)
  data <- expand.grid(time = 1:6, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), prob = 0.2, size = data$popn)
  formula <- deaths ~ sex + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit(mod)
  mod_disp <- make_mod_disp(mod)
  expect_setequal(names(mod_disp$priors), "(Intercept)")
  mu <- exp(make_linpred_raw(mod, point = TRUE))
  expect_true(mod_disp$mean_disp > 0)
})

test_that("'make_mod_disp' works with binom - large dataset", {
  set.seed(0)
  data <- expand.grid(time = 1:6000, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), prob = 0.3, size = data$popn)
  formula <- deaths ~ sex + time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn) |>
    set_prior(time ~ RW2(sd = 0))
  mod$point_effectfree <- rnorm(1 + 5999 + 2)
  mod$draws_effectfree  <- 1 ## to fool 'is_fitted'
  mod_disp <- make_mod_disp(mod)
  expect_true(identical(nrow(mod_disp$data), 10000L))
})

test_that("'make_mod_disp' works with norm", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$wt <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age * sex + sex * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  mod <- fit(mod)
  mod_disp <- make_mod_disp(mod)
  expect_setequal(names(mod_disp$priors), "(Intercept)")
  mu <- make_linpred_raw(mod, point = TRUE)
  expect_equal(mod_disp$outcome, mod$outcome - mu)
  expect_true(mod_disp$mean_disp > 0)
  expect_identical(length(mod_disp$dimnames_terms), 1L)
})

test_that("'make_mod_disp' works with norm - large dataset", {
  set.seed(0)
  data <- expand.grid(time = 2000:8000, sex = c("F", "M"))
  data$wt <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ time + sex
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  mod$point_effectfree <- rnorm(1 + 6001 + 2)
  mod_disp <- make_mod_disp(mod)
  expect_true(identical(nrow(mod_disp$data), 10000L))
})


## 'make_mod_inner' -----------------------------------------------------------

test_that("'make_mod_inner' works with pois", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rpois(n = nrow(data), lambda = 0.3 * data$popn)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  use_term <- make_use_term(mod, vars_inner = c("age", "sex"))
  ans_obtained <- make_mod_inner(mod, use_term)
  ans_expected <- reduce_model_terms(mod, use_term = use_term)
  ans_expected$mean_disp <- 0
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_mod_inner' works with norm", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$wt <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age * sex + sex * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  use_term <- make_use_term(mod, vars_inner = c("age", "sex"))
  ans_obtained <- make_mod_inner(mod, use_term = use_term)
  ans_expected <- reduce_model_terms(mod, use_term = use_term)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_mod_outer' -----------------------------------------------------------

test_that("'make_mod_outer' works with pois", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rpois(n = nrow(data), lambda = 0.3 * data$popn)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  use_term <- make_use_term(mod, vars_inner = c("age", "sex"))
  mod_inner <- reduce_model_terms(mod, use_term = use_term)
  mod_inner <- fit(mod_inner)
  mod_outer <- make_mod_outer(mod,
                              mod_inner = mod_inner,
                              use_term = use_term)
  expect_setequal(names(mod_outer$priors), c("time", "sex:time"))
  mu <- exp(make_linpred_raw(mod_inner, point = TRUE))
  expect_equal(mod_outer$offset, mod$offset * mu)
  expect_equal(mod_outer$mean_disp, 0)
})

test_that("'make_mod_outer' works with binom", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 1000)
  data$deaths <- rbinom(n = nrow(data), size =  data$popn, prob = 0.3)
  formula <- deaths ~ age * sex + sex * time
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  use_term <- make_use_term(mod, vars_inner = c("age", "sex"))
  mod_inner <- reduce_model_terms(mod, use_term = use_term)
  mod_inner <- fit(mod_inner)
  mod_outer <- make_mod_outer(mod,
                              mod_inner = mod_inner,
                              use_term = use_term)
  expect_setequal(names(mod_outer$priors), names(mod$priors))
  expect_s3_class(mod_outer$priors[["age"]], "bage_prior_known")
  expect_equal(mod_outer$offset, mod$offset)
  expect_equal(mod_outer$mean_disp, 0)
})

test_that("'make_mod_outer' works with norm", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$wt <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data))
  formula <- income ~ age * sex + sex * time
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  use_term <- make_use_term(mod, vars_inner = c("age", "sex"))
  mod_inner <- reduce_model_terms(mod, use_term = use_term)
  mod_inner <- fit(mod_inner)
  mod_outer <- make_mod_outer(mod,
                              mod_inner = mod_inner,
                              use_term = use_term)
  expect_setequal(names(mod_outer$priors), c("time", "sex:time"))
  mu <- make_linpred_raw(mod_inner, point = TRUE)
  expect_equal(mod_outer$outcome, mod$outcome - mu)
  expect_true(mod_outer$mean_disp > 0)
})


## 'make_par_disp' ---------------------------------------------------

test_that("'make_par_disp' works with bage_mod_pois - no NAs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  outcome <- data$deaths
  meanpar <- rvec::rpois_rvec(n = 120, lambda = outcome, n_draw = 5)
  disp <- rvec::runif_rvec(n = 1, min = 0.1, max = 0.5, n_draw = 5)
  set.seed(0)
  ans_obtained <- make_par_disp(mod,
                                meanpar = meanpar,
                                disp = disp)
  set.seed(0)
  ans_expected <- rvec::rgamma_rvec(n = length(meanpar),
                                    data$deaths + 1/disp,
                                    data$popn + 1/(disp*meanpar))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_par_disp' works with bage_mod_pois - has NAs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$popn[1] <- NA
  data$deaths[2] <- NA
  formula <- deaths ~ age + time + sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  meanpar <- rvec::rpois_rvec(n = 120, lambda = 100, n_draw = 5)
  disp <- rvec::runif_rvec(n = 1, min = 0.1, max = 0.5, n_draw = 5)
  set.seed(0)
  ans_obtained <- make_par_disp(mod,
                                meanpar = meanpar,
                                disp = disp)
  set.seed(0)
  data$popn[1:2] <- 0
  data$deaths[1:2] <- 0
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
  meanpar <- rvec::runif_rvec(n = 120, n_draw = 5)
  disp <- rvec::runif_rvec(n = 1, min = 0.1, max = 0.8, n_draw = 5)
  set.seed(0)
  ans_obtained <- make_par_disp(mod,
                                meanpar = meanpar,
                                disp = disp)
  set.seed(0)
  ans_expected <- rvec::rbeta_rvec(n = length(meanpar),
                                   data$deaths + meanpar/disp,
                                   data$popn - data$deaths + (1 - meanpar)/disp)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_par_disp' works with bage_mod_binom - has NAs", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
  data$popn[1] <- NA
  data$deaths[2] <- NA
  formula <- deaths ~ age + time + sex
  mod <- mod_binom(formula = formula,
                   data = data,
                   size = popn)
  outcome <- data$deaths
  offset <- data$popn
  meanpar <- rvec::runif_rvec(n = 120, n_draw = 5)
  disp <- rvec::runif_rvec(n = 1, min = 0.1, max = 0.8, n_draw = 5)
  set.seed(0)
  ans_obtained <- make_par_disp(mod,
                                meanpar = meanpar,
                                disp = disp)
  set.seed(0)
  data$popn[1:2] <- 0
  data$deaths[1:2] <- 0
  ans_expected <- rvec::rbeta_rvec(n = length(meanpar),
                                   data$deaths + meanpar/disp,
                                   data$popn - data$deaths + (1 - meanpar)/disp)
  expect_equal(ans_obtained, ans_expected)
})


## 'make_observed' ------------------------------------------------------------

test_that("'make_observed' works with bage_mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_observed(mod)
    ans_expected <- as.double(mod$outcome / mod$offset)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_observed' works with bage_mod_binom", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + sex
    mod <- mod_binom(formula = formula,
                     data = data,
                     size = popn)
    ans_obtained <- make_observed(mod)
    ans_expected <- as.double(mod$outcome / mod$offset)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'make_observed' throws expected with bage_mod_norm", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    expect_error(make_observed(mod),
                 "Internal error: `make_observed\\(\\)` called on object of class")
})


## 'model_descr' --------------------------------------------------------------

test_that("'model_descr' works with valid inputs", {
    expect_identical(model_descr(structure(1, class = "bage_mod_pois")), "Poisson")
    expect_identical(model_descr(structure(1, class = "bage_mod_binom")), "binomial")
    expect_identical(model_descr(structure(1, class = "bage_mod_norm")), "normal")
})


## 'nm_distn' -----------------------------------------------------------------

test_that("'nm_distn' works with valid inputs", {
    expect_identical(nm_distn(structure(1, class = "bage_mod_pois")), "pois")
    expect_identical(nm_distn(structure(1, class = "bage_mod_binom")), "binom")
    expect_identical(nm_distn(structure(1, class = "bage_mod_norm")), "norm")
})


## 'nm_offset' ----------------------------------------------------------------

test_that("'nm_offset' works with valid inputs", {
    expect_identical(nm_offset(structure(1, class = "bage_mod_pois")), "exposure")
    expect_identical(nm_offset(structure(1, class = "bage_mod_binom")), "size")
    expect_identical(nm_offset(structure(1, class = "bage_mod_norm")), "weights")
})


## 'print' --------------------------------------------------------------------

test_that("'print' works with mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- 3 * rpois(n = nrow(data), lambda = 0.4 * data$popn)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
      set_datamod_outcome_rr3()
    expect_snapshot(print(mod))
    ## don't use snapshot, since printed option includes timings, which can change
    capture.output(print(fit(mod)), file = NULL) 
})


test_that("'print' works with mod_pois - inner-outer fitting method", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- 3 * rpois(n = nrow(data), lambda = 0.4 * data$popn)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    expect_snapshot(print(mod))
    ## don't use snapshot, since printed option includes timings, which can change
    capture.output(print(fit(mod,
                             method = "inner-outer",
                             vars_inner = "age")),
                   file = NULL)
})


## 'replicate_data' -----------------------------------------------------------

test_that("'replicate_data' works with mod_pois", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 0.4 * data$popn)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$deaths, ans$.replicate, sd)
    expect_true(var(tab) > 0)
    ans_fit <- replicate_data(mod, condition_on = "fitted")
    expect_equal(mean(ans_fit$deaths), mean(ans$deaths), tolerance = 0.01)
})

test_that("'replicate_data' works with mod_pois, rr3 data model", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- 3 * rpois(n = nrow(data), lambda = 0.1 * data$popn)
    formula <- deaths ~ age + sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_datamod_outcome_rr3(mod)
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$deaths, ans$.replicate, sd)
    expect_true(var(tab) > 0)
    ans_fit <- replicate_data(mod, condition_on = "fitted")
    expect_equal(mean(ans_fit$deaths), mean(ans$deaths), tolerance = 0.02)
    expect_true(all(ans_fit$deaths %% 3 == 0))
})

test_that("'replicate_data' works with mod_binom", {
    set.seed(0)
    data <- expand.grid(age = 0:29, time = 2000:2002, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age + sex + time
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn)
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$deaths, ans$.replicate, mean)
    expect_true(var(tab) > 0)
    ans_fit <- replicate_data(mod, condition_on = "fitted")
    expect_equal(mean(ans_fit$deaths), mean(ans$deaths), tolerance = 0.01)
})

test_that("'replicate_data' works with mod_binom, rr3 data model", {
    set.seed(0)
    data <- expand.grid(age = 0:29, time = 2000:2002, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- 3 * rbinom(n = nrow(data), size = data$popn, prob = 0.1)
    formula <- deaths ~ age + sex + time
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn)
    mod <- set_datamod_outcome_rr3(mod)
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$deaths, ans$.replicate, mean)
    expect_true(var(tab) > 0)
    ans_fit <- replicate_data(mod, condition_on = "fitted")
    expect_equal(mean(ans_fit$deaths), mean(ans$deaths), tolerance = 0.03)
    expect_true(all(ans_fit$deaths %% 3 == 0))
})

test_that("'replicate_data' works with mod_norm", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$income <- rnorm(n = nrow(data))
    formula <- income ~ age + sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    mod <- set_prior(mod, age ~ N())
    mod <- set_prior(mod, time ~ N())
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$income, ans$.replicate, mean)
    expect_false(any(duplicated(tab)))
    expect_warning(replicate_data(mod, condition_on = "expected"),
                   "Ignoring value for `condition_on`.")
})


## 'tidy' ---------------------------------------------------------------------

test_that("'tidy' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rbinom(n = nrow(data), size = data$popn, prob = 0.3)
    formula <- deaths ~ age * sex
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn)
    ans_unfit <- tidy(mod)
    expect_true(is.data.frame(ans_unfit))
    expect_identical(names(ans_unfit), c("term", "prior", "along",
                                         "n_par", "n_par_free"))
    mod_fitted <- fit(mod)
    ans_fitted <- tidy(mod_fitted)
    expect_true(is.data.frame(ans_fitted))
    expect_identical(names(ans_fitted), c("term", "prior", "along",
                                          "n_par", "n_par_free",
                                          "std_dev"))
})
