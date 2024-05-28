## 'choose_matrices_along_by' -------------------------------------------------

test_that("'choose_matrices_along_by' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        time = 2000:2005,
                        region = 1:2,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time * region + sex * region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- choose_matrices_along_by(mod)
    matrices <- mod$matrices_along_by
    ans_expected <- list("(Intercept)" = matrices[[1]][[1L]],
                         age = matrices[[2]][[1L]],
                         sex = matrices[[3]][[1L]],
                         time = matrices[[4]][[1L]],
                         region = matrices[[5]][[1]],
                         "age:sex" = matrices[[6]][[1]],
                         "time:region" = matrices[[7]][[1]],
                         "sex:region" = matrices[[8]][[1]])
    expect_identical(ans_obtained, ans_expected)
})


## 'choose_matrix_along_by' ---------------------------------------------------

test_that("'choose_matrix_along_by' works with main effect", {
  prior <- N()
  matrices <- list(reg = matrix(0:3, nr = 4))
  var_time <- NULL
  var_age <- NULL
  ans_obtained <- choose_matrix_along_by(prior = prior,
                                         matrices = matrices,
                                         var_time = var_time,
                                         var_age = var_age)
  ans_expected <- matrices[[1L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'choose_matrix_along_by' works with interaction - default to time", {
  prior <- Lin()
  matrices <- list(age = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   time = make_matrix_along_by(i_along = 2L,
                                               dim = 2:4,
                                               dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   reg = make_matrix_along_by(i_along = 3L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- choose_matrix_along_by(prior = prior,
                                         matrices = matrices,
                                         var_time = var_time,
                                         var_age = var_age)
  ans_expected <- matrices[[2L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'choose_matrix_along_by' works with interaction - default to age", {
  prior <- Lin()
  matrices <- list(age = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   income = make_matrix_along_by(i_along = 2L,
                                                 dim = 2:4,
                                                 dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   reg = make_matrix_along_by(i_along = 3L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- choose_matrix_along_by(prior = prior,
                                         matrices = matrices,
                                         var_time = var_time,
                                         var_age = var_age)
  ans_expected <- matrices[[1L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'choose_matrix_along_by' works with interaction - default to first if along not used", {
  prior <- N()
  matrices <- list(gender = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   income = make_matrix_along_by(i_along = 2L,
                                                 dim = 2:4,
                                                 dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   reg = make_matrix_along_by(i_along = 3L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- choose_matrix_along_by(prior = prior,
                                         matrices = matrices,
                                         var_time = var_time,
                                         var_age = var_age)
  ans_expected <- matrices[[1L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'choose_matrix_along_by' works with interaction - specify along", {
  prior <- Lin(along = "reg")
  matrices <- list(age = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   income = make_matrix_along_by(i_along = 2L,
                                                 dim = 2:4,
                                                 dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   reg = make_matrix_along_by(i_along = 3L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- choose_matrix_along_by(prior = prior,
                                         matrices = matrices,
                                         var_time = var_time,
                                         var_age = var_age)
  ans_expected <- matrices[[3L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'choose_matrix_along_by' works when only one matrix", {
  prior <- Lin(along = "reg")
  matrices <- list(age = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- "time"
  var_age <- "age"
  ans_obtained <- choose_matrix_along_by(prior = prior,
                                         matrices = matrices,
                                         var_time = var_time,
                                         var_age = var_age)
  ans_expected <- matrices[[1L]]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'choose_matrix_along_by' throws expected error when can't find and time not specified", {
  prior <- Lin()
  matrices <- list(bla = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   time = make_matrix_along_by(i_along = 2L,
                                               dim = 2:4,
                                               dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   reg = make_matrix_along_by(i_along = 3L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- NULL
  var_age <- "age"
  expect_error(choose_matrix_along_by(prior = prior,
                                      matrices = matrices,
                                      var_time = var_time,
                                      var_age = var_age),
               "Prior for `bla:time:reg` does not have a value for `along`.")
})

test_that("'choose_matrix_along_by' throws expected error when can't find and time not specified", {
  prior <- Lin()
  matrices <- list(bla = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   time = make_matrix_along_by(i_along = 2L,
                                               dim = 2:4,
                                               dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   reg = make_matrix_along_by(i_along = 3L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- NULL
  var_age <- "age"
  expect_error(choose_matrix_along_by(prior = prior,
                                      matrices = matrices,
                                      var_time = var_time,
                                      var_age = var_age),
               "Prior for `bla:time:reg` does not have a value for `along`.")
})

test_that("'choose_matrix_along_by' throws expected error when can't find and age not specified", {
  prior <- Lin()
  matrices <- list(bla = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   time = make_matrix_along_by(i_along = 2L,
                                               dim = 2:4,
                                               dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   reg = make_matrix_along_by(i_along = 3L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- NULL
  var_age <- NULL
  expect_error(choose_matrix_along_by(prior = prior,
                                      matrices = matrices,
                                      var_time = var_time,
                                      var_age = var_age),
               "Prior for `bla:time:reg` does not have a value for `along`.")
})

test_that("'choose_matrix_along_by' throws expected error when along invalid", {
  prior <- Lin(along = "wrong")
  matrices <- list(age = make_matrix_along_by(i_along = 1L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   time = make_matrix_along_by(i_along = 2L,
                                               dim = 2:4,
                                               dimnames = list(a = 1:2, b = 1:3, c = 1:4)),
                   reg = make_matrix_along_by(i_along = 3L,
                                              dim = 2:4,
                                              dimnames = list(a = 1:2, b = 1:3, c = 1:4)))
  var_time <- "time"
  var_age <- "age"
  expect_error(choose_matrix_along_by(prior = prior,
                                      matrices = matrices,
                                      var_time = var_time,
                                      var_age = var_age),
               "Prior for `age:time:reg` has invalid value for `along`.")
})  


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
  set.seed(0)
  data <- expand.grid(agegp = 0:9,
                      time = 2000:2005,
                      region = 1:2,
                      SEX = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ agegp * SEX + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_agesex(mod)
  ans_expected <- list("(Intercept)" = "other",
                       agegp = "age",
                       SEX = "other",
                       region = "other",
                       "agegp:SEX" = "age:sex")
  expect_identical(ans_obtained, ans_expected)
})


## 'make_agesex_inner' --------------------------------------------------------

test_that("'make_agesex_inner' works with valid inputs", {
    expect_identical(make_agesex_inner("agegroup",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "age")
    expect_identical(make_agesex_inner("agegroup",
                                       var_age = NULL,
                                       var_sexgender = "gender"),
                     "other")
    expect_identical(make_agesex_inner("(Intercept)",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "other")
    expect_identical(make_agesex_inner("agegroup:gender",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "age:sex")
    expect_identical(make_agesex_inner("gender:agegroup",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "sex:age")
    expect_identical(make_agesex_inner("agegroup:gender",
                                       var_age = "agegroup",
                                       var_sexgender = NULL),
                     "age:other")
    expect_identical(make_agesex_inner("gender:agegroup:reg",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "sex:age:other")
    expect_identical(make_agesex_inner("region:agegroup",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "age:other")
    expect_identical(make_agesex_inner("gender:agegroup:region",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "sex:age:other")
    expect_identical(make_agesex_inner("agegroup:gender:region",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "age:sex:other")
    expect_identical(make_agesex_inner("agegroup:bla:region",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "age:other")
    expect_identical(make_agesex_inner("gender:agegroup:region",
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
  ans_expected <- rep(c("SEX:time" = 0), 2)
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


## 'make_is_time_varying' -----------------------------------------------------

test_that("'make_is_time_varying' works", {
  term <- c("(Intercept)", "age", "age", "time", "sex:time", "time:sex", "age:sex:time",
            "time", "sex:time", "sex:time")
  level <- c("(Intercept)", "0-4", "5+", "1999", "F.1999", "1999.F", "0.F.2020",
             "sd", "coef11", "mslope")
  var_time <- "time"
  ans_obtained <- make_is_time_varying(term = term, level = level, var_time = var_time)
  ans_expected <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
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
                    "age:sex" = 2L)
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
    matrices_effect_outcome <- make_matrices_effect_outcome(formula = formula, data = data)
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
    matrices_effect_outcome <- make_matrices_effect_outcome(formula = formula, data = data)
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
    matrices_effect_outcome <- make_matrices_effect_outcome(formula = formula, data = data)
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


## 'make_levels_forecast' -----------------------------------------------------

test_that("'make_levels_forecast' works with single time dimension", {
  set.seed(0)
  data <- expand.grid(age = 0:2,
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age + sex + time,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_levels_forecast(mod, labels_forecast = 2006:2007)
  ans_expected <- list("(Intercept)" = NULL,
                       age = NULL,
                       sex = NULL,
                       time = as.character(2006:2007))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_levels_forecast' works with single time dimension", {
  set.seed(0)
  data <- expand.grid(age = 0:2,
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age + sex * time,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_levels_forecast(mod, labels_forecast = 2006:2007)
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


## 'make_matrices_agesex' ---------------------------------------------------

test_that("'make_matrices_agesex' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(sex = c("F", "M"),
                        age = 0:3,
                        reg = c("A", "B"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(deaths ~ age * sex * reg,
                    data = data,
                    exposure = popn)
    ans <- make_matrices_agesex(mod)
    expect_identical(sapply(ans, is.null),
                     c("(Intercept)" = TRUE,
                       age = FALSE,
                       sex = TRUE,
                       reg = TRUE,
                       "age:sex" = FALSE,
                       "age:reg" = FALSE,
                       "sex:reg" = TRUE,
                       "age:sex:reg" = FALSE))
    expect_identical(ans[["age"]],
                     make_matrix_along_by(i = 1L,
                                          dim = 4, 
                                          dimnames = list(age = 0:3)))
    expect_identical(ans[["age:sex:reg"]],
                     make_matrix_along_by(i_along = 1:2,
                                          dim = c(4, 2, 2),
                                          dimnames = list(age = 0:3,
                                                          sex = c("F", "M"),
                                                          reg = c("A", "B"))))
})

test_that("'make_matrices_agesex' works with valid inputs - sex and age order reversed", {
    set.seed(0)
    data <- expand.grid(sex = c("F", "M"),
                        age = 0:3,
                        reg = c("A", "B"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    mod <- mod_pois(deaths ~ sex * reg * age,
                    data = data,
                    exposure = popn)
    ans <- make_matrices_agesex(mod)
    expect_identical(sapply(ans, is.null),
                     c("(Intercept)" = TRUE,
                       sex = TRUE,
                       reg = TRUE,
                       age = FALSE,
                       "sex:reg" = TRUE,
                       "sex:age" = FALSE,
                       "reg:age" = FALSE,
                       "sex:reg:age" = FALSE))
    expect_identical(ans[["age"]],
                     make_matrix_along_by(i = 1L,
                                          dim = 4, 
                                          dimnames = list(age = 0:3)))
    expect_identical(ans[["sex:reg:age"]],
                     make_matrix_along_by(i_along = c(1L, 3L),
                                          dim = c(2, 2, 4),
                                          dimnames = list(sex = c("F", "M"),
                                                          reg = c("A", "B"),
                                                          age = 0:3)))
})


## 'make_matrices_along_by' ---------------------------------------------------

test_that("'make_matrices_along_by' works with valid inputs", {
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
    ans_obtained <- make_matrices_along_by(formula = formula, data = data)
    age <- matrix(0:9, nr = 10)
    rownames(age) <- 0:9
    names(dimnames(age)) <- "age"
    sex <- matrix(0:1, nr = 2)
    rownames(sex) <- c("F", "M")
    names(dimnames(sex)) <- "sex"
    time <- matrix(0:1, nr = 2)
    rownames(time) <- 1:2
    names(dimnames(time)) <- "time"
    agesex <- matrix(0:19, nr = 10)
    dimnames(agesex) <- list(age = 0:9, sex = c("F", "M"))
    agetime <- matrix(0:19, nr = 10)
    dimnames(agetime) <- list(age = 0:9, time = 1:2)
    ans_expected <- list("(Intercept)" = list("(Intercept)" = matrix(0L, nr = 1L)),
                         age = list(age = age),
                         sex = list(sex = sex),
                         time = list(time = time),
                         "age:sex" = list(age = agesex,
                                          sex = t(agesex)),
                         "age:time" = list(age = agetime,
                                           time = t(agetime)))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_matrices_effect_outcome' --------------------------------------------

test_that("'make_matrices_effect_outcome' works with valid inputs", {
    data <- expand.grid(age = 0:5, time = 2000:2001, sex = 1:2)
    data$val <- 1
    data <- data[-c(3, 5), ]
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_matrices_effect_outcome(formula = formula, data = data)
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
    matrices <- make_matrices_agesex(mod)
    ans_obtained <- make_index_matrix(matrices[["reg:age:sex"]])
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
    m <- make_spline_matrix(n_along = 10, n_spline = 5)
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
    ans_expected <- factor(rep("sex:time", 2),
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


