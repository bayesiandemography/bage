## 'default_prior' ------------------------------------------------------------

test_that("'default_prior' works with ordinary term", {
    expect_identical(default_prior(nm_term = "x",
                                   var_age = "age",
                                   var_time = "time",
                                   length_effect = 5L),
                     N())
})

test_that("'default_prior' works with term with length 1", {
    expect_identical(default_prior(nm_term = "(Intercept)",
                                   var_age = "age",
                                   var_time = "time",
                                   length_effect = 1L),
                     NFix())
})

test_that("'default_prior' works with term with length 2", {
    expect_identical(default_prior(nm_term = "reg",
                                   var_age = "age",
                                   var_time = "time",
                                   length_effect = 1L),
                     NFix())
})


test_that("'default_prior' works with age main effect", {
    expect_identical(default_prior(nm_term = "AgeGroup",
                                   var_age = "AgeGroup",
                                   var_time = "time",
                                   length_effect = 5),
                     RW())
    expect_identical(default_prior(nm_term = "AgeGroup",
                                   var_age = "AgeGroup",
                                   var_time = NULL,
                                   length_effect = 5),
                     RW())
    expect_identical(default_prior(nm_term = "AgeGroup",
                                   var_age = NULL,
                                   var_time = NULL,
                                   length_effect = 5),
                     N())
})

test_that("'default_prior' works with age interaction", {
    expect_identical(default_prior(nm_term = "AgeGroup:sex",
                                   var_age = "AgeGroup",
                                   var_time = "time",
                                   length_effect = 5),
                     RW())
    expect_identical(default_prior(nm_term = "time:AgeGroup",
                                   var_age = "AgeGroup",
                                   var_time = "time",
                                   length_effect = 5),
                     RW())
    expect_identical(default_prior(nm_term = "period:AgeGroup",
                                   var_age = NULL,
                                   var_time = NULL,
                                   length_effect = 5),
                     N())
})

test_that("'default_prior' works with time main effect term", {
    expect_identical(default_prior(nm_term = "year",
                                   var_age = "AgeGroup",
                                   var_time = "year",
                                   length_effect = 5),
                     RW())
})

test_that("'default_prior' works with time interaction", {
    expect_identical(default_prior(nm_term = "sex:year",
                                   var_age = "AgeGroup",
                                   var_time = "year",
                                   length_effect = 5),
                     RW())
    expect_identical(default_prior(nm_term = "sex:year:region",
                                   var_age = "AgeGroup",
                                   var_time = "year",
                                   length_effect = 5),
                     RW())
})


## 'dimnames_to_levels' -------------------------------------------------------

test_that("'dimnames_to_levels' works with 0D dimnames", {
  dimnames <- list()
  ans_obtained <- dimnames_to_levels(dimnames)
  ans_expected <- "(Intercept)"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'dimnames_to_levels' works with 1D dimnames", {
  dimnames <- list(age = 0:4)
  ans_obtained <- dimnames_to_levels(dimnames)
  ans_expected <- as.character(0:4)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'dimnames_to_levels' works with 2D dimnames", {
  dimnames <- list(age = 0:4, reg = c("a", "b"))
  ans_obtained <- dimnames_to_levels(dimnames)
  ans_expected <- paste(0:4, rep(c("a", "b"), each = 5), sep = ".")
  expect_identical(ans_obtained, ans_expected)
})


## 'dimnames_to_nm' -------------------------------------------------------

test_that("'dimnames_to_nm' works with 0D dimnames", {
  dimnames <- list()
  ans_obtained <- dimnames_to_nm(dimnames)
  ans_expected <- "(Intercept)"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'dimnames_to_nm' works with 1D dimnames", {
  dimnames <- list(age = 0:4)
  ans_obtained <- dimnames_to_nm(dimnames)
  ans_expected <- "age"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'dimnames_to_nm' works with 2D dimnames", {
  dimnames <- list(age = 0:4, reg = c("a", "b"))
  ans_obtained <- dimnames_to_nm(dimnames)
  ans_expected <- "age:reg"
  expect_identical(ans_obtained, ans_expected)
})


## 'dimnames_to_nm_split' -----------------------------------------------------

test_that("'dimnames_to_nm_split' works with 0D dimnames", {
  dimnames <- list()
  ans_obtained <- dimnames_to_nm_split(dimnames)
  ans_expected <- "(Intercept)"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'dimnames_to_nm_split' works with 1D dimnames", {
  dimnames <- list(age = 0:4)
  ans_obtained <- dimnames_to_nm_split(dimnames)
  ans_expected <- "age"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'dimnames_to_nm_split' works with 2D dimnames", {
  dimnames <- list(age = 0:4, reg = c("a", "b"))
  ans_obtained <- dimnames_to_nm_split(dimnames)
  ans_expected <- c("age", "reg")
  expect_identical(ans_obtained, ans_expected)
})


## 'eval_offset_formula' ------------------------------------------------------

test_that("'eval_offset_formula' works with valid inputs - simple formula", {
  vname_offset <- "~popn + other"
  data <- data.frame(popn = 1, other = 2)
  ans_obtained <- eval_offset_formula(vname_offset = vname_offset, data = data)
  ans_expected <- 3
  expect_identical(ans_obtained, ans_expected)
})

test_that("'eval_offset_formula' works with valid inputs - complicated formula", {
  vname_offset <- "~popn^2 + log(other) + 6"
  data <- data.frame(popn = 1:2, other = 2:3)
  ans_obtained <- eval_offset_formula(vname_offset = vname_offset, data = data)
  ans_expected <- (1:2)^2 + log(2:3) + 6
  expect_identical(ans_obtained, ans_expected)
})


test_that("'eval_offset_formula' works with valid inputs - ifelse", {
  vname_offset <- "~ifelse(popn <= 0, 0.1, popn)"
  data <- data.frame(popn = 0:2)
  ans_obtained <- eval_offset_formula(vname_offset = vname_offset, data = data)
  ans_expected <- c(0.1, 1, 2)
  expect_identical(ans_obtained, ans_expected)
})


## 'get_n_comp_spline' --------------------------------------------------------

test_that("'get_n_comp_spline' works with n_comp supplied", {
  expect_identical(get_n_comp_spline(Sp(n_comp = 4), n_along = 10), 4L)
})

test_that("'get_n_comp_spline' works with n_comp supplied", {
  expect_identical(get_n_comp_spline(Sp(), n_along = 10), 7L)
})


## 'get_print_prior_n_offset' -------------------------------------------------

test_that("'get_print_prior_n_offset' works", {
  expect_identical(get_print_prior_n_offset(), 8L)
})

  
## 'infer_var_age' ------------------------------------------------------------

test_that("'infer_var_age' returns name when single valid answer", {
    expect_identical(infer_var_age(deaths ~ age * sex + time),
                     "age")
    expect_identical(infer_var_age(deaths ~ age * sex + time + age),
                     "age")
    expect_identical(infer_var_age(deaths ~ Age * sex + time),
                     "Age")
    expect_identical(infer_var_age(deaths ~ AGE_GROUP * sex + time),
                     "AGE_GROUP")
    expect_identical(infer_var_age(deaths ~ agegroup * sex + time),
                     "agegroup")
    expect_identical(infer_var_age(deaths ~ ageinterval * sex + time),
                     "ageinterval")
    expect_identical(infer_var_age(deaths ~ age.years * sex + time),
                     "age.years")
    expect_identical(infer_var_age(deaths ~ age.year * sex + time),
                     "age.year")
})

test_that("'infer_var_age' returns NULL when not single valid answer", {
    expect_identical(infer_var_age(deaths ~ agex * sex + time),
                     NULL)
    expect_identical(infer_var_age(deaths ~ sex + time),
                     NULL)
    expect_identical(infer_var_age(deaths ~ 1),
                     NULL)
})


## 'infer_var_sexgender' ------------------------------------------------------------

test_that("'infer_var_sexgender' returns name when single valid answer", {
    expect_identical(infer_var_sexgender(deaths ~ age * sex + time),
                     "sex")
    expect_identical(infer_var_sexgender(deaths ~ age:gender + time + age),
                     "gender")
})

test_that("'infer_var_sexgender' returns NULL when not single valid answer", {
    expect_identical(infer_var_sexgender(deaths ~ age * sex + gender),
                     NULL)
    expect_identical(infer_var_sexgender(deaths ~ age + time),
                     NULL)
    expect_identical(infer_var_sexgender(deaths ~ 1),
                     NULL)
})


## 'infer_var_time' -----------------------------------------------------------

test_that("'infer_var_time' returns name when single valid answer", {
    expect_identical(infer_var_time(deaths ~ time * sex + age),
                     "time")
    expect_identical(infer_var_time(deaths ~ Time * sex + age),
                     "Time")
    expect_identical(infer_var_time(deaths ~ PERIOD * sex + age),
                     "PERIOD")
    expect_identical(infer_var_time(deaths ~ QUARters * sex + age),
                     "QUARters")
    expect_identical(infer_var_time(deaths ~ month * sex + age),
                     "month")
    expect_identical(infer_var_time(deaths ~ years * sex + age),
                     "years")
    expect_identical(infer_var_time(deaths ~ year * sex + age),
                     "year")
    expect_identical(infer_var_time(deaths ~ sex + month_year),
                     "month_year")
    expect_identical(infer_var_time(deaths ~ sex + year_quarter),
                     "year_quarter")
    expect_identical(infer_var_time(deaths ~ sex + quarter_year),
                     "quarter_year")
})

test_that("'infer_var_time' returns NULL when not single valid answer", {
    expect_identical(infer_var_time(deaths ~ xTime + sex + age),
                     NULL)
    expect_identical(infer_var_time(deaths ~ time * sex + year_month),
                     NULL)
    expect_identical(infer_var_time(deaths ~ age * sex + PERIODX),
                     NULL)
    expect_identical(infer_var_time(deaths ~ 1),
                     NULL)
})


## 'make_agesex' --------------------------------------------------------------

test_that("'make_agesex' works with valid inputs", {
  expect_identical(make_agesex("agegroup",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "age")
  expect_identical(make_agesex("agegroup",
                               var_age = NULL,
                               var_sexgender = "gender"),
                   "other")
  expect_identical(make_agesex("(Intercept)",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "other")
  expect_identical(make_agesex("agegroup:gender",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "age:sex")
  expect_identical(make_agesex("gender:agegroup",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "sex:age")
  expect_identical(make_agesex("agegroup:gender",
                               var_age = "agegroup",
                               var_sexgender = NULL),
                   "age:other")
  expect_identical(make_agesex("gender:agegroup:reg",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "sex:age:other")
  expect_identical(make_agesex("region:agegroup",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "age:other")
  expect_identical(make_agesex("gender:agegroup:region",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "sex:age:other")
  expect_identical(make_agesex("agegroup:gender:region",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "age:sex:other")
  expect_identical(make_agesex("agegroup:bla:region",
                               var_age = "agegroup",
                               var_sexgender = "gender"),
                   "age:other")
  expect_identical(make_agesex("gender:agegroup:region",
                               var_age = NULL,
                               var_sexgender = NULL),
                   "other")
})


## 'make_const' --------------------------------------------------------------- 

test_that("'make_const' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
        set_prior((Intercept) ~ Known(3))
    ans_obtained <- make_const(mod)
    ans_expected <- c("(Intercept)" = 0,
                      agegp.scale = 1,
                      SEX.sd = 1)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_dimnames_terms' ------------------------------------------------------

test_that("'make_dimnames_terms' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        time = 2000:2005,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    ans_obtained <- make_dimnames_terms(data = data, formula = formula)
    ans_expected <- list("(Intercept)" = list(),
                         age = list(age = as.character(0:9)),
                         sex = list(sex = c("F", "M")),
                         time = list(time = as.character(2000:2005)),
                         "age:sex" = list(age = as.character(0:9), sex = c("F", "M")))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_effectfree' -------------------------------------------------------------

test_that("'make_effectfree' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
        set_prior((Intercept) ~ Known(3))
    ans_obtained <- make_effectfree(mod)
    ans_expected <- c("(Intercept)" = 3,
                      agegp = 0, agegp = 0, agegp = 0,
                      SEX = 0, SEX = 0)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_hyper' ---------------------------------------------------------------

test_that("'make_hyper' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(agegp = 0:2,
                      SEX = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ agegp * SEX
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_hyper(mod)
  ans_expected <- c(agegp = 0, "agegp:SEX" = 0)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_hyperrand' ---------------------------------------------------------------

test_that("'make_hyperrand' works with valid inputs - no hyperrand", {
  set.seed(0)
  data <- expand.grid(agegp = 0:2,
                      SEX = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ agegp * SEX
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_hyperrand(mod)
  ans_expected <- numeric()
  names(ans_expected) <- character()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_hyperrand' works with valid inputs - has hyperrand", {
  set.seed(0)
  data <- expand.grid(agegp = 0:2,
                      SEX = c("F", "M"),
                      time = 2001:2005)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ agegp + SEX * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, SEX:time ~ Lin())
  ans_obtained <- make_hyperrand(mod)
  ans_expected <- rep(c("SEX:time" = 0), 4)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_i_prior' -------------------------------------------------------------

test_that("'make_i_prior' works with valid inputs", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()))
    ans_obtained <- make_i_prior(mod)
    ans_expected <- c(a = 4L, b = 6L, c = 4L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_is_in_lik' -----------------------------------------------------------

test_that("'make_is_in_lik' works with no NAs", {
    mod <- list(outcome = c(0, 1, 5),
                offset = c(1, 0, 3))
    ans_obtained <- make_is_in_lik(mod)
    ans_expected <- c(1L, 0L, 1L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_is_in_lik' works with NAs", {
    mod <- list(outcome = c(0, 1, NA, 7),
                offset = c(1, 0, 3, NA))
    ans_obtained <- make_is_in_lik(mod)
    ans_expected <- c(1L, 0L, 0L, 0L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_lengths_effect' ---------------------------------------------------------

test_that("'make_lengths_effect' works with valid inputs", {
    matrices_effect_outcome = list(a = matrix(nr = 100, nc = 1),
                                b = matrix(nr = 100, nc = 5),
                                c = matrix(nr = 100, nc = 5))
    ans_obtained <- make_lengths_effect(matrices_effect_outcome)
    ans_expected <- c(a = 1L, b = 5L, c = 5L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_lengths_effectfree' -----------------------------------------------------------

test_that("'make_lengths_effectfree' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_lengths_effectfree(mod)
    ans_expected <- c("(Intercept)" = 1L,
                      agegp = 10L,
                      SEX = 2L,
                      region = 2L,
                      "agegp:SEX" = 20L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_lengths_hyper' -------------------------------------------------------

test_that("'make_lengths_hyper' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:3,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ Lin())
  mod <- set_prior(mod, sex ~ NFix())
  ans_obtained <- make_lengths_hyper(mod)
  ans_expected <- c("(Intercept)" = 0L,
                    age = 1L,
                    sex = 0L,
                    region = 1L,
                    "age:sex" = 1L)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_lengths_hyperrand' -------------------------------------------------------

test_that("'make_lengths_hyperrand' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:sex ~ Lin())
  mod <- set_prior(mod, sex ~ NFix())
  ans_obtained <- make_lengths_hyperrand(mod)
  ans_expected <- c("(Intercept)" = 0L,
                    age = 0L,
                    sex = 0L,
                    region = 0L,
                    "age:sex" = 4L)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_age' ----------------------------------------------------------

test_that("'make_levels_age' works when data has age variable", {
  set.seed(0)
  data <- expand.grid(age = c("infant", "1 to 4", "5-9", "10 plus"),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age * sex + time,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_levels_age(mod)
  ans_expected <- unique(data$age)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_age' works when data has no age variable", {
  set.seed(0)
  data <- expand.grid(bla = c("infant", "1 to 4", "5-9", "10 plus"),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ bla * sex + time,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_levels_age(mod)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_effect' ----------------------------------------------------------

test_that("'make_levels_effect' works with valid inputs - pois, complete levels", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    dimnames_terms <- make_dimnames_terms(formula = formula, data = data)
    matrices_effect_outcome <- make_matrices_effect_outcome(data = data,
                                                            dimnames_terms = dimnames_terms)
    ans_obtained <- make_levels_effect(matrices_effect_outcome = matrices_effect_outcome)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'make_levels_effect' works with valid inputs - pois, incomplete levels", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data <- data[-3, ]
    formula <- deaths ~ age * sex + time
    dimnames_terms <- make_dimnames_terms(formula = formula, data = data)
    matrices_effect_outcome <- make_matrices_effect_outcome(data = data,
                                                            dimnames_terms = dimnames_terms)
    ans_obtained <- make_levels_effect(matrices_effect_outcome = matrices_effect_outcome)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'make_levels_effect' works with valid inputs - norm", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$income <- rnorm(n = nrow(data))
    formula <- income ~ age * sex + time
    dimnames_terms <- make_dimnames_terms(formula = formula, data = data)
    matrices_effect_outcome <- make_matrices_effect_outcome(data = data,
                                                            dimnames_terms = dimnames_terms)
    ans_obtained <- make_levels_effect(matrices_effect_outcome = matrices_effect_outcome)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
    expect_identical(ans_obtained, ans_expected)                      
})


## 'make_levels_forecast_all' -------------------------------------------------

test_that("'make_levels_forecast_all' works with single time dimension", {
  set.seed(0)
  data <- expand.grid(age = 0:2,
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age + sex + time,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_levels_forecast_all(mod, labels_forecast = 2006:2007)
  ans_expected <- list("(Intercept)" = NULL,
                       age = NULL,
                       sex = NULL,
                       time = as.character(2006:2007))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_forecast_all' works with single time dimension", {
  set.seed(0)
  data <- expand.grid(age = 0:2,
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age + sex * time,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_levels_forecast_all(mod, labels_forecast = 2006:2007)
  ans_expected <- list("(Intercept)" = NULL,
                       age = NULL,
                       sex = NULL,
                       time = as.character(2006:2007),
                       "sex:time" = paste(c("F", "M"), c(2006, 2006, 2007, 2007), sep = "."))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_sexgender' ----------------------------------------------------------

test_that("'make_levels_sexgender' works when data has sexgender variable", {
  set.seed(0)
  data <- expand.grid(age = c("infant", "1 to 4", "5-9", "10 plus"),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age * sex + time,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_levels_sexgender(mod)
  ans_expected <- unique(data$sex)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_sexgender' works when data has no sexgender variable", {
  set.seed(0)
  data <- expand.grid(age = c("infant", "1 to 4", "5-9", "10 plus"),
                      time = 2000:2005,
                      bla = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age * bla + time,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_levels_sexgender(mod)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
})


## 'make_map' -----------------------------------------------------------------

test_that("'make_map' works with no parameters fixed", {
    set.seed(0)
    data <- expand.grid(time = 2000:2009,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_map(mod)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_map' works when 'effectfree' contains known values", {
    set.seed(0)
    data <- expand.grid(time = 0:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, SEX ~ Known(c(0.1, -0.1)))
    ans_obtained <- make_map(mod)
    ans_expected <- list(effectfree = factor(c("(Intercept)" = 1,
                                            time = 2,
                                            time = 3,
                                            time = 4,
                                            time = 5,
                                            SEX = NA,
                                            SEX = NA,
                                            "time:SEX" = 6,
                                            "time:SEX" = 7,
                                            "time:SEX" = 8,
                                            "time:SEX" = 9,
                                            "time:SEX" = 10,
                                            "time:SEX" = 11,
                                            "time:SEX" = 12,
                                            "time:SEX" = 13)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_map' works dispersion is 0", {
    set.seed(0)
    data <- expand.grid(time = 2000:2009,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_disp(mod, mean = 0)
    ans_obtained <- make_map(mod)
    ans_expected <- list(log_disp = factor(NA))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_map' works when effectfree has known values", {
    set.seed(0)
    data <- expand.grid(time = 0:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, SEX ~ Known(c(0.1, -0.1)))
    ans_obtained <- make_map(mod)
    ans_expected <- list(effectfree = factor(c("(Intercept)" = 1,
                                            time = 2,
                                            time = 3,
                                            time = 4,
                                            time = 5,
                                            SEX = NA,
                                            SEX = NA,
                                            "time:SEX" = 6,
                                            "time:SEX" = 7,
                                            "time:SEX" = 8,
                                            "time:SEX" = 9,
                                            "time:SEX" = 10,
                                            "time:SEX" = 11,
                                            "time:SEX" = 12,
                                            "time:SEX" = 13)))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_map_effectfree_fixed' ---------------------------------------------------

test_that("'make_map_effectfree_fixed' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(time = 0:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, SEX ~ Known(c(-1, 1)))
    ans_obtained <- make_map_effectfree_fixed(mod)
    ans_expected <- factor(c("(Intercept)" = 1,
                             time = 2,
                             time = 3,
                             time = 4,
                             SEX = NA,
                             SEX = NA))
    expect_identical(ans_obtained, ans_expected)
    expect_identical(length(make_map_effectfree_fixed(mod)),
                     length(make_effectfree(mod)))
})


## 'make_matrices_along_by_effectfree' ------------------------------------------------

test_that("'make_matrices_along_by_effectfree' works", {
  set.seed(0)
  data <- expand.grid(agegp = 0:9,
                      region = 1:2,
                      SEX = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ agegp * SEX + SEX * region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, agegp ~ Sp(n_comp = 4))
  ans <- make_matrices_along_by_effectfree(mod)
  expect_true(all(sapply(ans, is.matrix)))
  expect_identical(names(ans), names(mod$priors))
})

test_that("'make_matrices_along_by_effectfree' works - with SVD", {
  set.seed(0)
  data <- expand.grid(age = c(0:59, "60+"),
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + age * time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, age:time ~ SVD_RW(HMD))
  ans <- make_matrices_along_by_effectfree(mod)
  expect_true(all(sapply(ans, is.matrix)))
  expect_identical(names(ans), names(mod$priors))
})


## 'make_matrices_along_by_forecast' ------------------------------------------

test_that("'make_matrices_along_by_forecast' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        time = 1:2,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex  + age * time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_matrices_along_by_forecast(mod = mod,
                                                    labels_forecast = 3:4)
    time <- matrix(0:1, nr = 2)
    rownames(time) <- 3:4
    names(dimnames(time)) <- "time"
    agetime <- t(matrix(0:19, nr = 10))
    dimnames(agetime) <- list(time = 3:4, age = 0:9)
    ans_expected <- list("(Intercept)" = NULL,
                         age = NULL,
                         sex = NULL,
                         time = time,
                         "age:sex" = NULL,
                         "age:time" = agetime)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_matrices_effect_outcome' --------------------------------------------

test_that("'make_matrices_effect_outcome' works with valid inputs", {
    data <- expand.grid(age = 0:5, time = 2000:2001, sex = 1:2)
    data$val <- 1
    data <- data[-c(3, 5), ]
    formula <- deaths ~ age:sex + time
    dimnames_terms <- make_dimnames_terms(formula = formula, data = data)
    ans_obtained <- make_matrices_effect_outcome(data = data, dimnames_terms = dimnames_terms)
    data_fac <- data[1:3]
    data_fac[] <- lapply(data_fac, factor)
    ans_expected <- Matrix::sparse.model.matrix(~age:sex + time,
                                                data = data_fac,
                                                contrasts.arg = lapply(data_fac,
                                                                       contrasts,
                                                                       contrast = FALSE),
                                                row.names = FALSE)
    v <- rnorm(n = ncol(ans_expected))
    expect_equal(do.call(cbind, ans_obtained) %*% v,
                 ans_expected %*% v)
    expect_identical(names(ans_obtained), c("(Intercept)", "time", "age:sex"))
})


## 'make_matrices_effectfree_effect' ------------------------------------------------

test_that("'make_matrices_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_matrices_effectfree_effect(mod)
    ans_expected <- list("(Intercept)" = Matrix::.sparseDiagonal(1),
                         agegp = Matrix::.sparseDiagonal(10),
                         SEX = Matrix::.sparseDiagonal(2),
                         region = Matrix::.sparseDiagonal(2),
                         "agegp:SEX" = Matrix::.sparseDiagonal(20))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_index_matrix' -------------------------------------------------

test_that("'make_index_matrix' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(sex = c("F", "M"),
                      age = 0:3,
                      reg = c("A", "B"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ reg * age * sex,
                  data = data,
                  exposure = popn)
  matrix_agesex <- make_matrix_agesex(dimnames_term = mod$dimnames_terms[["reg:age:sex"]],
                                      var_age = mod$var_age,
                                      var_sexgender = mod$var_sexgender)
  ans_obtained <- make_index_matrix(matrix_agesex)
  cn <- paste(0:3, rep(c("F", "M"), each = 4), rep(c("A", "B"), each = 8), sep = ".")
  rn <- paste(rep(0:3, each = 2), rep(c("F", "M"), each = 8), c("A", "B"), sep = ".")
  ans_expected <- Matrix::sparseMatrix(i = c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 15L,
                                             2L, 4L, 6L, 8L, 10L, 12L, 14L, 16L),
                                       j = 1:16,
                                       x = rep(1L, 16),
                                       dimnames = list(rn, cn))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_along_by' -----------------------------------------------------

test_that("'make_matrix_along_by' works when 'i_along' is 1", {
  i_along <- 1L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(0:23,
                         nr = 2,
                         dimnames = list(a = 1:2,
                                         b.c = paste(1:3, rep(1:4, each = 3), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when 'i_along' is 2", {
  i_along <- 2L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(c(0L, 2L, 4L,
                           1L, 3L, 5L,
                           6L, 8L, 10L,
                           7L, 9L, 11L,
                           12L, 14L, 16L,
                           13L, 15L, 17L,
                           18L, 20L, 22L,
                           19L, 21L, 23L),
                         nr = 3,
                         dimnames = list(b = 1:3,
                                         a.c = paste(1:2, rep(1:4, each = 2), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when 'i_along' is 3", {
  i_along <- 3L
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(c(0L, 6L, 12L, 18L,
                           1L, 7L, 13L, 19L,
                           2L, 8L, 14L, 20L,
                           3L, 9L, 15L, 21L,
                           4L, 10L, 16L, 22L,
                           5L, 11L, 17L, 23L),
                         nrow = 4,
                         dimnames = list(c = 1:4,
                                         a.b = paste(1:2, rep(1:3, each = 2), sep = ".")))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when only one dimension", {
  i_along <- 1L
  dim <- 3L
  dimnames <- list(a = 1:3)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(0:2, nr = 3)
  rownames(ans_expected) <- 1:3
  names(dimnames(ans_expected))[1] <- "a"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when only one element", {
  i_along <- 1L
  dim <- 1L
  dimnames <- list(a = 1)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(0L, nr = 1)
  rownames(ans_expected) <- 1
  names(dimnames(ans_expected))[1] <- "a"
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when 'i_along' is 1:2", {
  i_along <- 1:2
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(0:23,
                         nr = 6,
                         dimnames = list(a.b = paste(1:2, rep(1:3, each = 2), sep = "."),
                                         c = 1:4))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_along_by' works when 'i_along' is 1:2", {
  i_along <- c(1L, 3L)
  dim <- 2:4
  dimnames <- list(a = 1:2, b = 1:3, c = 1:4)
  ans_obtained <- make_matrix_along_by(i_along = i_along,
                                       dim = dim,
                                       dimnames = dimnames)
  ans_expected <- matrix(c(0L, 1L, 6L, 7L, 12L, 13L, 18L, 19L,
                           2L, 3L, 8L, 9L, 14L, 15L, 20L, 21L,
                           4L, 5L, 10L, 11L, 16L, 17L, 22L, 23L),
                         nr = 8,
                         dimnames = list(a.c = paste(1:2, rep(1:4, each = 2), sep = "."),
                                         b = 1:3))
  expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_effectfree_effect_svd' ----------------------------------------

test_that("'make_matrix_effectfree_effect_svd' works with bage_prior_svd - age main effect", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected),
                                       dimnames = dimnames(ans_expected))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_svd' works with bage_prior_svd - age-sex interaction, joint", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3, indep = FALSE)
  dimnames_term <- list(sex = c("Female", "Male"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$matrix[s$data$type == "joint"][[1L]][c(1,3,2,4),1:3]
  ans_expected <- Matrix::sparseMatrix(i = row(ans_expected),
                                       j = col(ans_expected),
                                       x = as.double(ans_expected))
  rownames(ans_expected) <- c("Female.0-4", "Male.0-4", "Female.5-9", "Male.5-9")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_svd' works with bage_prior_svd - age x reg interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 3)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        x = 1:2)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  m2 <- s$data$matrix[s$data$type == "total"][[1L]][,1:3]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  m1 <- make_index_matrix(matrix_agesex)
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_effectfree_effect_svd' works with bage_prior_svd - sex x reg x age interaction", {
  prior <- SVD(HMD)
  dimnames_term = list(sex = c("F", "M"),
                       age = c(0, "1-4", paste(seq(5, 55, 5), seq(9, 59, 5), sep = "--"), "60+"),
                       reg <- c("A", "B"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_matrix_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  m2 <- HMD$data$matrix[[35]][as.integer(t(matrix(1:28,nr=14))), c(1:3, 6:8)]
  m2 <- Matrix::kronecker(Matrix::.sparseDiagonal(2), m2)
  matrix_agesex <- make_matrix_agesex(dimnames_term = dimnames_term,
                                      var_age = var_age,
                                      var_sexgender = var_sexgender)
  m1 <- make_index_matrix(matrix_agesex)
  ans_expected <- m1 %*% m2
  expect_identical(ans_obtained, ans_expected)
})


## 'make_offset' --------------------------------------------------------------

test_that("'make_offset' works with valid inputs - no NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$wt <- seq_len(nrow(data))
    ans_obtained <- make_offset(vname_offset = "wt",
                                data = data)
    ans_expected <- as.double(data$wt)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset' works with valid inputs - has NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$wt <- seq_len(nrow(data))
    data$wt[3] <- NA
    ans_obtained <- make_offset(vname_offset = "wt",
                                data = data)
    ans_expected <- xtabs(wt ~ age + sex + time, data = data)
    ans_expected[3] <- NA
    ans_expected <- as.double(data$wt)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset' works with valid inputs - no NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$wt <- seq_len(nrow(data))
    ans_obtained <- make_offset(vname_offset = "~ wt + 1",
                                data = data)
    ans_expected <- as.double(data$wt) + 1
    expect_identical(ans_obtained, ans_expected)
})



## 'make_offset_effectfree_effect_svd' ----------------------------------------

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- rep(ans_expected, 3)
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD_AR1(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- rep(ans_expected, 3)
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD_RW(ssvd = s, n_comp = 2)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- rep(ans_expected, 3)
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - age x time interaction", {
  s <- sim_ssvd()
  prior <- SVD_RW2(ssvd = s, n_comp = 2)
  levels_effect = c("0-4.2001", "5-9.2001", "0-4.2002", "5-9.2002", "0-4.2003", "5-9.2003")
  dimnames_term <- list(age = c("0-4", "5-9"),
                        time = 2001:2003)
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "total"][[1L]]
  ans_expected <- rep(ans_expected, 3)
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - sex x age interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2)
  levels_effect = c("F.0-4", "M.0-4", "F.5-9", "M.5-9")
  dimnames_term <- list(sex = c("F", "M"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "indep"][[1L]][c(1,3,2,4)]
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset_effectfree_effect_svd' works with bage_prior_svd - sex x age interaction", {
  s <- sim_ssvd()
  prior <- SVD(ssvd = s, n_comp = 2, indep = FALSE)
  levels_effect = c("F.0-4", "M.0-4", "F.5-9", "M.5-9")
  dimnames_term <- list(sex = c("F", "M"),
                        age = c("0-4", "5-9"))
  var_time <- "time"
  var_age <- "age"
  var_sexgender <- "sex"
  ans_obtained <- make_offset_effectfree_effect_svd(prior = prior,
                                                    dimnames_term = dimnames_term,
                                                    var_time = var_time,
                                                    var_age = var_age,
                                                    var_sexgender = var_sexgender)
  ans_expected <- s$data$offset[s$data$type == "joint"][[1L]][c(1,3,2,4)]
  names(ans_expected) <- levels_effect
  expect_identical(ans_obtained, ans_expected)
})


## 'make_offset_ones' -----------------------------------------------------

test_that("'make_offset_ones' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- 1:12
    ans_obtained <- make_offset_ones(data)
    ans_expected <- rep(1.0, times = 12)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_offsets_effectfree_effect' ------------------------------------------------

test_that("'make_offsets_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_offsets_effectfree_effect(mod)
    ans_expected <- list("(Intercept)" = 0,
                         agegp = rep(0, 10),
                         SEX = rep(0, 2),
                         region = rep(0, 2),
                         "agegp:SEX" = rep(0, 20))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_outcome' -------------------------------------------------------------

test_that("'make_outcome' works with valid inputs", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$deaths <- seq_len(nrow(data))
    data$deaths[3] <- NA
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_outcome(formula = formula,
                                 data = data)
    ans_expected <- as.double(data$deaths)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_priors' --------------------------------------------------------------

test_that("'make_priors' works with valid inputs - has intercept", {
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_priors(formula,
                                var_age = "age",
                                var_time = "time",
                                lengths_effect = c(1L, 10L, 12L))
    ans_expected <- list("(Intercept)" = NFix(),
                         time = RW(),
                         "age:sex" = RW())
    expect_identical(ans_obtained, ans_expected)
})


## 'make_random' --------------------------------------------------------------

test_that("'make_random' works when no hyper, no hyperrand", {
    mod <- structure(.Data = list(priors = list(NFix(), Known(c(2, 3)))))
    expect_identical(make_random(mod), NULL)
})

test_that("'make_random' works when hyper, no hyperrand", {
    mod <- structure(.Data = list(priors = list(N(), RW2())))
    expect_identical(make_random(mod), "effectfree")
})

test_that("'make_random' works when hyper, hyperrand", {
    mod <- structure(.Data = list(priors = list(N(), RW2(), Lin())))
    expect_identical(make_random(mod), c("effectfree", "hyperrand"))
})


## 'make_seed' --------------------------------------------------------------

test_that("'make_seed' returns a single unique integer", {
    set.seed(0)
    ans1 <- make_seed()
    ans2 <- make_seed()
    expect_true(is.integer(ans1))
    expect_identical(length(ans1), 1L)
    expect_false(ans1 == ans2)
})


## 'make_spline_matrix' -------------------------------------------------------

test_that("'make_spline_matrix' works", {
    set.seed(0)
    m <- make_spline_matrix(n_along = 10, n_comp = 5)
    expect_equal(dim(m), c(10L, 5L))
    expect_equal(colSums(as.matrix(m)), rep(0, times = 5))
})        


## 'make_terms_const' ---------------------------------------------------------

test_that("'make_terms_const' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, agegp ~ AR1())
    ans_obtained <- make_terms_const(mod)
    ans_expected <- factor(c("(Intercept)", rep("agegp", 5), "SEX", "region", "agegp:SEX"),
                           levels = c("(Intercept)", "agegp", "SEX", "region", "agegp:SEX"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_effect' -----------------------------------------------------------

test_that("'make_terms_effect' works with valid inputs", {
    matrices_effect_outcome = list(a = matrix(nr = 100, nc = 1),
                                b = matrix(nr = 100, nc = 5),
                                c = matrix(nr = 100, nc = 5))
    ans_obtained <- make_terms_effect(matrices_effect_outcome)
    ans_expected <- factor(rep(c("a", "b", "c"),
                               times = c(1, 5, 5)))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_effectfree' -----------------------------------------------------------

test_that("'make_terms_effectfree' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_terms_effectfree(mod)
    ans_expected <- factor(c("(Intercept)",
                             rep("agegp", times = 10),
                             rep("SEX", times = 2),
                             rep("region", times = 2),
                             rep("agegp:SEX", times = 20)),
                           levels = c("(Intercept)",
                                      "agegp",
                                      "SEX",
                                      "region",
                                      "agegp:SEX"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_hyper' ---------------------------------------------------------

test_that("'make_terms_hyper' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_terms_hyper(mod)
    ans_expected <- factor(c("agegp", "region", "agegp:SEX"),
                           levels = c("(Intercept)", "agegp", "SEX", "region", "agegp:SEX"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_hyperrand' ---------------------------------------------------------

test_that("'make_terms_hyperrand' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex*time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ Lin())
    ans_obtained <- make_terms_hyperrand(mod)
    ans_expected <- factor(rep("sex:time", 4),
                           levels = c("(Intercept)",
                                      "age",
                                      "sex",
                                      "time",
                                      "sex:time"))
    expect_identical(ans_obtained, ans_expected)                      
})


## 'make_uses_hyper' ----------------------------------------------------------

test_that("'make_uses_hyper' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) %>%
        set_prior(agegp ~ NFix())
    ans_obtained <- make_uses_hyper(mod)
    ans_expected <- c("(Intercept)" = 0L,
                      agegp = 0L,
                      SEX = 0L,
                      region = 1L,
                      "agegp:SEX" = 1L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_uses_hyperrand' ------------------------------------------------------

test_that("'make_uses_hyperrand' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex*time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ Lin())
    ans_obtained <- make_uses_hyperrand(mod)
    ans_expected <- c("(Intercept)" = 0L,
                      age = 0L,
                      sex = 0L,
                      time = 0L,
                      "sex:time" = 1L)
    expect_identical(ans_obtained, ans_expected)                      
})


## 'make_uses_matrix_effectfree_effect' ---------------------------------------

test_that("'make_uses_matrix_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) %>%
        set_prior(agegp ~ Sp())
    ans_obtained <- make_uses_matrix_effectfree_effect(mod)
    ans_expected <- c("(Intercept)" = 0L,
                      agegp = 1L,
                      SEX = 0L,
                      region = 0L,
                      "agegp:SEX" = 0L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_uses_matrix_effectfree_effect' ---------------------------------------------

test_that("'make_uses_offset_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp * SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) %>%
        set_prior(agegp ~ Sp())
    ans_obtained <- make_uses_offset_effectfree_effect(mod)
    ans_expected <- c("(Intercept)" = 0L,
                      agegp = 0L,
                      SEX = 0L,
                      region = 0L,
                      "agegp:SEX" = 0L)
    expect_identical(ans_obtained, ans_expected)
})


## 'n_comp_svd' ---------------------------------------------------------------

test_that("'n_comp_svd' works when no 'n' supplied", {
  ans_obtained <- n_comp_svd(n_comp = NULL, nm_n_comp = "n", ssvd = HMD)
  ans_expected <- 3L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'n_comp_svd' works when valid 'n' supplied", {
  ans_obtained <- n_comp_svd(n_comp = 3, nm_n_comp = "n", ssvd = HMD)
  ans_expected <- 3L
  expect_identical(ans_obtained, ans_expected)
})

test_that("'n_comp_svd' throws correct error when n is too high", {
  expect_error(n_comp_svd(n_comp = 11, nm_n_comp = "n_component", ssvd = HMD),
               "`n_component` larger than number of components of `ssvd`.")
})


## 'print_prior' -------------------------------------------------------

test_that("'print_prior' works", {
  expect_snapshot(print_prior(RW(),
                              nms = c("s", "along"),
                              slots = c("scale", "along")))
})


## 'print_prior_header' -------------------------------------------------------

test_that("'print_prior_header' works", {
  expect_snapshot(print_prior_header(AR()))
})


## 'print_prior_slot' ---------------------------------------------------------

test_that("'print_prior_slot' works", {
  expect_snapshot(print_prior_slot(AR(), nm = "n_coef", slot = "n_coef"))
})


## 'str_call_args_along' ---------------------------------------------------------

test_that("'str_call_args_along' works - no along", {
  prior <- RW()
  ans_obtained <- str_call_args_along(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_along' works - has along", {
  prior <- RW(along = "cohort")
  ans_obtained <- str_call_args_along(prior)
  ans_expected <- "along=\"cohort\""
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_ar' ---------------------------------------------------------

test_that("'str_call_args_ar' works - AR1", {
  prior <- AR1(s = 0.5)
  ans_obtained <- str_call_args_ar(prior)
  ans_expected <- c("", "", "s=0.5")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_svd' works - AR", {
  prior <- AR(n_coef = 3)
  ans_obtained <- str_call_args_ar(prior)
  ans_expected <- c("n_coef=3", "")
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_lin' --------------------------------------------------------

test_that("'str_call_args_lin' works - sd_slope = 1", {
  prior <- Lin()
  ans_obtained <- str_call_args_lin(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_lin' works - sd_slope not 1", {
  prior <- Lin(sd = 0.3)
  ans_obtained <- str_call_args_lin(prior)
  ans_expected <- "sd=0.3"
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_n_comp' --------------------------------------------------------

test_that("'str_call_args_n_comp' works - no n_comp", {
  prior <- Sp()
  ans_obtained <- str_call_args_n_comp(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_n_comp' works - n_comp provided", {
  prior <- Sp(n_comp=5)
  ans_obtained <- str_call_args_n_comp(prior)
  ans_expected <- "n_comp=5"
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_n_seas' --------------------------------------------------------

test_that("'str_call_args_n_seas' works", {
  prior <- RW_Seas(n_seas=3)
  ans_obtained <- str_call_args_n_seas(prior)
  ans_expected <- "n_seas=3"
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_s_seas' ------------------------------------------------------

test_that("'str_call_args_s_seas' works - s_seas = 1", {
  prior <- RW_Seas(n=3)
  ans_obtained <- str_call_args_s_seas(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_s_seas' works - s_seas not 1", {
  prior <- RW_Seas(n=2,s_seas = 0.3)
  ans_obtained <- str_call_args_s_seas(prior)
  ans_expected <- "s_seas=0.3"
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_scale' ------------------------------------------------------

test_that("'str_call_args_scale' works - scale = 1", {
  prior <- N()
  ans_obtained <- str_call_args_scale(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_scale' works - scale not 1", {
  prior <- N(s = 0.3)
  ans_obtained <- str_call_args_scale(prior)
  ans_expected <- "s=0.3"
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_svd' --------------------------------------------------------

test_that("'str_call_args_svd' works - total", {
  prior <- SVD(HMD)
  ans_obtained <- str_call_args_svd(prior)
  ans_expected <- c("HMD", "", "")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_svd' works - indep", {
  prior <- SVD(HMD, n_comp = 2)
  ans_obtained <- str_call_args_svd(prior)
  ans_expected <- c("HMD", "n_comp=2", "")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_svd' works - joint", {
  prior <- SVD(HMD, indep = FALSE, n_comp = 3)
  ans_obtained <- str_call_args_svd(prior)
  ans_expected <- c("HMD", "", "indep=FALSE")
  expect_identical(ans_obtained, ans_expected)
})


## 'to_factor' ----------------------------------------------------------------

test_that("'to_factor' leaves existing factor unchanged", {
  x <- factor(letters)
  expect_identical(to_factor(x), x)
})

test_that("'to_factor' orders numeric x by values", {
  x <- c(3, 1, 0.2, 1)
  expect_identical(to_factor(x), factor(x, levels = c(0.2, 1, 3)))
})


test_that("'to_factor' orders non-numeric non-factor by order of appearance", {
  x <- c("b", "a", 1, "a")
  expect_identical(to_factor(x), factor(x, levels = c("b", "a", 1)))
})


