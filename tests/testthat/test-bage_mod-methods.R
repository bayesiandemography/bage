
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


## 'draw_vals_augment_fitted' ---------------------------------------------------------------

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
  data$deaths <- rr3(rpois(n = nrow(data), lambda = 10))
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
  data$deaths <- rr3(rbinom(n = nrow(data), size = data$popn, prob = 0.4))
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
  vals_components <- draw_vals_components_unfitted(mod, n_sim = 10,
                                                   standardize = "anova")
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
                                                   n_sim = n_sim,
                                                   standardize = "anova")
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
                                                   n_sim = n_sim,
                                                   standardize = "none")
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
                                                   n_sim = n_sim,
                                                   standardize = "none")
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
                                                   n_sim = n_sim,
                                                   standardize = "anova")
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
    mod <- set_prior(mod, age ~ RW())
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
    expect_equal(ans_obtained$est$effectfree[[1L]], -2)
    expect_equal(ans_obtained$est$effectfree[names(ans_obtained$est$effectfree) == "sex"], c(sex = -0.1, sex = 0.1))
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
    expect_false(is.null(mod_fitted$est))
})

test_that("'fit' works when model consists of intercept only", {
    data <- data.frame(deaths = 1:10,
                       age = rep(1:2, each = 5),
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ 1,
                    data = data,
                    exposure = 1)
    mod_fitted <- fit(mod)
    expect_false(is.null(mod_fitted$est))
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
    expect_false(is.null(mod_fitted$est))
})

test_that("'fit' works when single dimension", {
    data <- data.frame(deaths = 1:10,
                       time = 2001:2010)
    mod <- mod_pois(deaths ~ time,
                    data = data,
                    exposure = 1)
    mod_fitted <- fit(mod)
    expect_identical(length(mod_fitted$est$effectfree), nrow(data) + 1L)
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
    mod <- set_prior(mod, time ~ Lin_AR())
    ans_obtained <- fit(mod)
    expect_s3_class(ans_obtained, "bage_mod")
})

test_that("'fit' and 'forecast' work with SVD_AR", {
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      time = 2001:2010)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- NA
  mod <- mod_pois(deaths ~ age:time,
                  data = data,
                  exposure = population) |>
                  set_prior(age:time ~ SVD_AR(LFP))
  mod <- fit(mod)
  expect_true(is_fitted(mod))
  f <- forecast(mod, labels = 2011:2012)
  expect_setequal(c(names(f), ".deaths"),
                  names(augment(mod)))
})

test_that("'fit' and 'forecast' work with SVD_RW", {
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      time = 2001:2010)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- NA
  mod <- mod_pois(deaths ~ age:time,
                  data = data,
                  exposure = population) |>
                  set_prior(age:time ~ SVD_RW(LFP))
  mod <- fit(mod)
  expect_true(is_fitted(mod))
  f <- forecast(mod, labels = 2011:2012)
  expect_setequal(c(names(f), ".deaths"),
                  names(augment(mod)))
})

test_that("'fit' and 'forecast' work with SVD_RW2", {
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      time = 2001:2010)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- NA
  mod <- mod_pois(deaths ~ age:time,
                  data = data,
                  exposure = population) |>
                  set_prior(age:time ~ SVD_RW2(LFP))
  mod <- fit(mod)
  expect_true(is_fitted(mod))
  f <- forecast(mod, labels = 2011:2012)
  expect_setequal(c(names(f), ".deaths"),
                  names(augment(mod)))
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
  ans_unstand_est <- forecast(mod,
                              labels = 2005:2006,
                              output = "comp",
                              standardize = "none",
                              include_estimates = TRUE)
  expect_identical(names(ans_est), names(ans_unstand_est))
  ans_unstand_no_est <- forecast(mod,
                                 labels = 2005:2006,
                                 output = "comp",
                                 standardize = "none",
                                 include_estimates = FALSE)
  expect_identical(names(ans_no_est), names(ans_unstand_no_est))
  ans_anova <- forecast(mod,
                        labels = 2005:2006,
                        output = "comp",
                        standardize = "anova",
                        include_estimates = TRUE)
  expect_identical(names(ans_anova), names(ans_unstand_est))
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


## 'forecast_augment' --------------------------------------------------------

test_that("'forecast_augment' works - Poisson, has disp", {
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
  data_forecast <- make_data_forecast(mod = mod, labels_forecast = labels_forecast)
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
  data_forecast <- make_data_forecast(mod= mod, labels_forecast = labels_forecast)
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
  data_forecast <- make_data_forecast(mod= mod, labels_forecast = labels_forecast)
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


## 'has_disp' -----------------------------------------------------------------

test_that("'is_fitted' works with valid inputs", {
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

test_that("'make_i_lik_mod' works with bage_mod_binom", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$wt <- rpois(n = nrow(data), lambda = 1000)
  data$income <- rnorm(n = nrow(data), sd = 10)
  formula <- income ~ age + time + sex
  mod <- mod_norm(formula = formula,
                  data = data,
                  weights = wt)
  expect_identical(make_i_lik_mod(mod), 201L)
  mod$nm_datamod <- "wrong"
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
    mod <- fit(mod)
    expect_snapshot(print(mod))
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
    mod <- set_n_draw(mod, 5)
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
    mod <- set_n_draw(mod, 5)
    mod <- set_datamod_outcome_rr3(mod)
    mod <- fit(mod)
    ans <- replicate_data(mod)
    expect_identical(names(ans), c(".replicate", names(data)))
    expect_identical(nrow(ans), nrow(data) * 20L)
    tab <- tapply(ans$deaths, ans$.replicate, sd)
    expect_true(var(tab) > 0)
    ans_fit <- replicate_data(mod, condition_on = "fitted")
    expect_equal(mean(ans_fit$deaths), mean(ans$deaths), tolerance = 0.01)
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
    formula <- deaths ~ age + sex
    mod <- mod_binom(formula = formula,
                    data = data,
                    size = popn)
    ans_unfit <- tidy(mod)
    expect_true(is.data.frame(ans_unfit))
    expect_identical(names(ans_unfit), c("term", "spec", "n"))
    mod_fitted <- fit(mod)
    ans_fitted <- tidy(mod_fitted)
    expect_true(is.data.frame(ans_fitted))
    expect_identical(names(ans_fitted), c("term", "spec", "n", "sd"))
})
