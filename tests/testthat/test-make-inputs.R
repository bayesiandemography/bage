
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
  nm_offset_data <- "~popn + other"
  data <- data.frame(popn = 1, other = 2)
  ans_obtained <- eval_offset_formula(nm_offset_data = nm_offset_data, data = data)
  ans_expected <- 3
  expect_identical(ans_obtained, ans_expected)
})

test_that("'eval_offset_formula' works with valid inputs - complicated formula", {
  nm_offset_data <- "~popn^2 + log(other) + 6"
  data <- data.frame(popn = 1:2, other = 2:3)
  ans_obtained <- eval_offset_formula(nm_offset_data = nm_offset_data, data = data)
  ans_expected <- (1:2)^2 + log(2:3) + 6
  expect_identical(ans_obtained, ans_expected)
})


test_that("'eval_offset_formula' works with valid inputs - ifelse", {
  nm_offset_data <- "~ifelse(popn <= 0, 0.1, popn)"
  data <- data.frame(popn = 0:2)
  ans_obtained <- eval_offset_formula(nm_offset_data = nm_offset_data, data = data)
  ans_expected <- c(0.1, 1, 2)
  expect_identical(ans_obtained, ans_expected)
})


## 'get_matrix_offset_svd_prior' ----------------------------------------------


test_that("'get_matrix_or_offset_svd_prior' works with age main effect, type is total, matrix", {
  ssvd <- sim_ssvd()
  prior <- SVD(ssvd, n_comp = 3)
  ans_obtained <- get_matrix_or_offset_svd_prior(prior = prior,
                                                 dimnames_term = list(age = c("0-4", "5-9")),
                                                 var_age = "age",
                                                 var_sexgender = "sex",
                                                 get_matrix = TRUE)
  ans_expected <- Matrix::Matrix(1, nr = 2, nc = 3, 
                                 dimnames = list(c("0-4", "5-9"), NULL))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd_prior' works with age main effect, type is total, offset", {
  ssvd <- sim_ssvd()
  prior <- SVD(ssvd, n_comp = 3)
  ans_obtained <- get_matrix_or_offset_svd_prior(prior = prior,
                                                 dimnames_term = list(age = c("0-4", "5-9")),
                                                 var_age = "age",
                                                 var_sexgender = "sex",
                                                 get_matrix = FALSE)
  ans_expected <- c("0-4" = 1, "5-9" = 2)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd_prior' works with sex-age interaction, type is joint, offset", {
  ssvd <- sim_ssvd()
  prior <- SVD(ssvd, indep = FALSE)
  dimnames_term <- list(age = c("0-4", "5-9"),
                        sex = c("Male", "Female"))
  ans_obtained <- get_matrix_or_offset_svd_prior(prior = prior,
                                                 dimnames_term = dimnames_term,
                                                 var_age = "age",
                                                 var_sexgender = "sex",
                                                 get_matrix = FALSE)
  ans_expected <- c("Male.0-4" = 3, "Male.5-9" = 4, "Female.0-4" = 1, "Female.5-9" = 2)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'get_matrix_or_offset_svd_prior' works with age-sex interaction, type is indep, matrix", {
  ssvd <- sim_ssvd()
  prior <- SVD(ssvd)
  dimnames_term <- list(sex = c("Female", "Male"),
                        age = c("0-4", "5-9"))
  ans_obtained <- get_matrix_or_offset_svd_prior(prior,
                                                 dimnames_term = dimnames_term,
                                                 var_age = "age",
                                                 var_sexgender = "sex",
                                                 get_matrix = TRUE)
  ans_expected <- Matrix::Matrix(3, nr = 4, nc = 10,
                                 dimnames = list(c("Female.0-4", "Male.0-4",
                                                   "Female.5-9", "Male.5-9"),
                                                 NULL))
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
  expect_identical(get_print_prior_n_offset(), 10L)
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


## 'get_is_in_lik' ----------------------------------------------------------------

test_that("'get_is_in_lik' works with no NAs", {
    set.seed(0)
    data <- expand.grid(age = 0:2,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- rnorm(n = nrow(data))
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- get_is_in_lik(mod)
    ans_expected <- rep(TRUE, 6)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_is_in_lik' works with NAs", {
    set.seed(0)
    data <- expand.grid(age = c(0:1, NA),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$popn[1] <- 0
    data$deaths[1] <- 0
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- get_is_in_lik(mod)
    ans_expected <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
    expect_identical(ans_obtained, ans_expected)
})


## 'get_is_in_lik_covariates' --------------------------------------------------------

test_that("'get_is_in_lik_covariates' works with no NAs", {
    set.seed(0)
    data <- expand.grid(age = 0:2,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- rnorm(n = nrow(data))
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
      set_covariates(~ income)
    ans_obtained <- get_is_in_lik_effects(mod)
    ans_expected <- rep(TRUE, 6)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_is_in_lik_effects' works with NAs", {
    set.seed(0)
    data <- expand.grid(age = c(0:1, NA),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- get_is_in_lik_effects(mod)
    ans_expected <- rep(c(TRUE, TRUE, FALSE), 2)
    expect_identical(ans_obtained, ans_expected)
})


## 'get_is_in_lik_effects' --------------------------------------------------------

test_that("'get_is_in_lik_effects' works with no NAs", {
    set.seed(0)
    data <- expand.grid(age = 0:2,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- get_is_in_lik_effects(mod)
    ans_expected <- rep(TRUE, 6)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_is_in_lik_effects' works with NAs", {
    set.seed(0)
    data <- expand.grid(age = c(0:1, NA),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- get_is_in_lik_effects(mod)
    ans_expected <- rep(c(TRUE, TRUE, FALSE), 2)
    expect_identical(ans_obtained, ans_expected)
})


## 'get_is_in_lik_offset' -----------------------------------------------------

test_that("'get_is_in_lik_offset' works with no NAs", {
    mod <- list(outcome = c(0, 1, 5),
                offset = c(1, 0, 3))
    ans_obtained <- get_is_in_lik_offset(mod)
    ans_expected <- c(TRUE, FALSE, TRUE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_is_in_lik_offset' works with NAs", {
    mod <- list(outcome = c(0, 1, NA, 7),
                offset = c(1, 0, 3, NA))
    ans_obtained <- get_is_in_lik_offset(mod)
    ans_expected <- c(TRUE, FALSE, TRUE, FALSE)
    expect_identical(ans_obtained, ans_expected)
})


## 'get_is_in_lik_outcome' ----------------------------------------------------

test_that("'get_is_in_lik_outcome' works with no NAs", {
    mod <- list(outcome = c(0, 1, 5),
                offset = c(1, 0, 3))
    ans_obtained <- get_is_in_lik_outcome(mod)
    ans_expected <- c(TRUE, TRUE, TRUE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'get_is_in_lik_outcome' works with NAs", {
    mod <- list(outcome = c(0, 1, NA, 7),
                offset = c(1, 0, 3, NA))
    ans_obtained <- get_is_in_lik_outcome(mod)
    ans_expected <- c(TRUE, TRUE, FALSE, TRUE)
    expect_identical(ans_obtained, ans_expected)
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


## 'make_coef_covariates' -----------------------------------------------------

test_that("'make_coef_covariates' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        region = c("a", "b"),
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$income <- runif(n = nrow(data))
    data$distance <- runif(n = nrow(data))
    mod <- mod_pois(formula = deaths ~ age * sex,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_coef_covariates(mod)
    ans_expected <- double()
    expect_identical(ans_obtained, ans_expected)
    mod <- set_covariates(mod, ~ income + distance)
    ans_obtained <- make_coef_covariates(mod)
    ans_expected <- c(income = 0, distance = 0)
    expect_identical(ans_obtained, ans_expected)
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
                      agegp.sd = 1,
                      SEX.sd = 1)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_const' works with valid inputs - no terms", {
    set.seed(0)
    data <- expand.grid(agegp = 0:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- reduce_model_terms(mod, use_term = rep(F, 3))
    ans_obtained <- make_const(mod)
    ans_expected <- double()
    expect_identical(ans_obtained, ans_expected)
})


## 'make_data_df' -------------------------------------------------------------

test_that("'make_data_df' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        time = 2000:2005,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data$deaths[1] <- NA
    mod <- mod_pois(deaths ~ age * sex + time,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_data_df(mod)
    ans_expected <- data[-1,]
    ans_expected$deaths <- as.double(ans_expected$deaths)
    ans_expected$popn <- as.double(ans_expected$popn)
    ans_expected <- tibble::tibble(ans_expected)
    expect_identical(ans_obtained, ans_expected)    
})


## 'make_dimnames_terms' ------------------------------------------------------

test_that("'make_dimnames_terms' works - includes intercept", {
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

test_that("'make_dimnames_terms' works - no intercept", {
    set.seed(0)
    data <- expand.grid(age = 0:9,
                        time = 2000:2005,
                        sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time - 1
    ans_obtained <- make_dimnames_terms(data = data, formula = formula)
    ans_expected <- list(age = list(age = as.character(0:9)),
                         sex = list(sex = c("F", "M")),
                         time = list(time = as.character(2000:2005)),
                         "age:sex" = list(age = as.character(0:9), sex = c("F", "M")))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_effectfree' ----------------------------------------------------------

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

test_that("'make_effectfree' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- reduce_model_terms(mod, use_term = rep(F, 3))
    ans_obtained <- make_effectfree(mod)
    ans_expected <- double()
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


## 'make_hyperrandfree' -------------------------------------------------------

test_that("'make_hyperrandfree' works with valid inputs - no hyperrandfree", {
  set.seed(0)
  data <- expand.grid(agegp = 0:2,
                      SEX = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ agegp * SEX
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_hyperrandfree(mod)
  ans_expected <- numeric()
  names(ans_expected) <- character()
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_hyperrandfree' works with valid inputs - has hyperrandfree", {
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
  ans_obtained <- make_hyperrandfree(mod)
  ans_expected <- rep(c("SEX:time" = 0), 2)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_i_prior' -------------------------------------------------------------

test_that("'make_i_prior' works with valid inputs", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()))
    ans_obtained <- make_i_prior(mod)
    ans_expected <- c(a = 4L, b = 19L, c = 4L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_lengths_effect' ------------------------------------------------------

test_that("'make_lengths_effect' works with valid inputs - has intercept", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time
  dimnames_terms <- make_dimnames_terms(data = data, formula = formula)
  ans_obtained <- make_lengths_effect(dimnames_terms)
  ans_expected <- c("(Intercept)" = 1L,
                    age = 10L,
                    sex = 2L,
                    time = 6L,
                    "age:sex" = 20L)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_lengths_effect' works with valid inputs - no intercept", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + time - 1
  dimnames_terms <- make_dimnames_terms(data = data, formula = formula)
  ans_obtained <- make_lengths_effect(dimnames_terms)
  ans_expected <- c(age = 10L,
                    sex = 2L,
                    time = 6L,
                    "age:sex" = 20L)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_lengths_effectfree' --------------------------------------------------

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


## 'make_lengths_hyperrandfree' -----------------------------------------------

test_that("'make_lengths_hyperrandfree' works with valid inputs", {
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
  ans_obtained <- make_lengths_hyperrandfree(mod)
  ans_expected <- c("(Intercept)" = 0L,
                    age = 0L,
                    sex = 0L,
                    region = 0L,
                    "age:sex" = 2L)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_effects' ----------------------------------------------------------

test_that("'make_levels_effects' works with valid inputs - pois, complete levels", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    dimnames_terms <- make_dimnames_terms(formula = formula, data = data)
    ans_obtained <- make_levels_effects(dimnames_terms)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'make_levels_effects' works with valid inputs - pois, incomplete levels", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data <- data[-3, ]
    formula <- deaths ~ age * sex + time
    dimnames_terms <- make_dimnames_terms(formula = formula, data = data)
    ans_obtained <- make_levels_effects(dimnames_terms)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'make_levels_effects' works with valid inputs - norm", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$income <- rnorm(n = nrow(data))
    formula <- income ~ age * sex + time
    dimnames_terms <- make_dimnames_terms(formula = formula, data = data)
    ans_obtained <- make_levels_effects(dimnames_terms)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'make_levels_effects' works with valid inputs - no terms", {
    ans_obtained <- make_levels_effects(list())
    ans_expected <- character()
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

test_that("'make_levels_forecast_all' works with time interaction", {
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

test_that("'make_levels_forecast_all' works with no intercept", {
  set.seed(0)
  data <- expand.grid(age = 0:2,
                      time = 2000:2005,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age + sex + time,
                  data = data,
                  exposure = popn)
  mod <- reduce_model_terms(mod, use_term = c(F, T, T, T))
  ans_obtained <- make_levels_forecast_all(mod, labels_forecast = 2006:2007)
  ans_expected <- list(age = NULL,
                       sex = NULL,
                       time = as.character(2006:2007))
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

test_that("'make_matrices_along_by_forecast' works with intercept", {
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

test_that("'make_matrices_along_by_forecast' works with no intercept", {
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
    mod <- reduce_model_terms(mod, use_term = c(F, T, T, T, F, F))
    ans_obtained <- make_matrices_along_by_forecast(mod = mod,
                                                    labels_forecast = 3:4)
    time <- matrix(0:1, nr = 2)
    rownames(time) <- 3:4
    names(dimnames(time)) <- "time"
    ans_expected <- list(age = NULL,
                         sex = NULL,
                         time = time)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_matrices_effect_outcome' --------------------------------------------

test_that("'make_matrices_effect_outcome' works with valid inputs - has intercept", {
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

test_that("'make_matrices_effect_outcome' works with valid inputs - no intercept", {
    data <- expand.grid(age = 0:5, time = 2000:2001, sex = 1:2)
    data$val <- 1
    data <- data[-c(3, 5), ]
    formula <- deaths ~ age:sex + time - 1
    dimnames_terms <- make_dimnames_terms(formula = formula, data = data)
    ans_obtained <- make_matrices_effect_outcome(data = data, dimnames_terms = dimnames_terms)
    data_fac <- data[1:3]
    data_fac[] <- lapply(data_fac, factor)
    ans_expected <- Matrix::sparse.model.matrix(~age:sex + time - 1,
                                                data = data_fac,
                                                contrasts.arg = lapply(data_fac,
                                                                       contrasts,
                                                                       contrast = FALSE),
                                                row.names = FALSE)
    v <- rnorm(n = ncol(ans_expected))
    expect_equal(do.call(cbind, ans_obtained) %*% v,
                 ans_expected %*% v)
    expect_identical(names(ans_obtained), c("time", "age:sex"))
})


## 'make_matrices_effectfree_effect' ------------------------------------------------

test_that("'make_matrices_effectfree_effect' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:9,
                        region = 1:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp + SEX + region
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) |>
      set_prior(agegp ~ RW(sd = 0))
    ans_obtained <- make_matrices_effectfree_effect(mod)
    agegp <- rbind(0,Matrix::.sparseDiagonal(9))
    ans_expected <- list("(Intercept)" = Matrix::.sparseDiagonal(1),
                         agegp = agegp,
                         SEX = Matrix::.sparseDiagonal(2),
                         region = Matrix::.sparseDiagonal(2))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_covariates' ---------------------------------------------------

test_that("'make_matrix_covariates' works with valid inputs - all numeric", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = c("a", "b"),
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- runif(n = nrow(data))
  data$distance <- runif(n = nrow(data))
  formula <- ~ income + distance
  ans_obtained <- make_matrix_covariates(formula = formula, data = data)
  ans_expected <- model.matrix(~income + distance - 1, data = data)
  ans_expected[,"income"] <- scale(ans_expected[,"income"])
  ans_expected[,"distance"] <- scale(ans_expected[,"distance"])
  rownames(ans_expected) <- NULL
  attributes(ans_expected)$assign <- NULL
  rownames(ans_expected) <- NULL
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_matrix_covariates' works with valid inputs - not all numeric - has intercept", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = c("a", "b"),
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- runif(n = nrow(data))
  formula <- ~ income * region
  ans_obtained <- make_matrix_covariates(formula = formula, data = data)
  data_scaled <- data
  data_scaled$income <- as.numeric(scale(data_scaled$income))
  ans_expected <- model.matrix(~income*region, data = data_scaled)[,-1]
  rownames(ans_expected) <- NULL
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_covariates' works with valid inputs - not all numeric - no intercept", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = c("a", "b"),
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- runif(n = nrow(data))
  formula <- ~ income * region - 1
  ans_obtained <- make_matrix_covariates(formula = formula, data = data)
  data_scaled <- data
  data_scaled$income <- as.numeric(scale(data_scaled$income))
  ans_expected <- model.matrix(~income*region, data = data_scaled)[,-1]
  attr(ans_expected, "assign") <- NULL
  rownames(ans_expected) <- NULL
  expect_identical(ans_obtained, ans_expected)
})


## 'make_offset' --------------------------------------------------------------

test_that("'make_offset' works with valid inputs - no NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$wt <- seq_len(nrow(data))
    ans_obtained <- make_offset(nm_offset_data = "wt",
                                data = data)
    ans_expected <- as.double(data$wt)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset' works with valid inputs - has NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$wt <- seq_len(nrow(data))
    data$wt[3] <- NA
    ans_obtained <- make_offset(nm_offset_data = "wt",
                                data = data)
    ans_expected <- xtabs(wt ~ age + sex + time, data = data)
    ans_expected[3] <- NA
    ans_expected <- as.double(data$wt)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_offset' works with valid inputs - no NA", {
    data <- expand.grid(age = 0:2, time = 2000:2001, sex = 1:2)
    data$wt <- seq_len(nrow(data))
    ans_obtained <- make_offset(nm_offset_data = "~ wt + 1",
                                data = data)
    ans_expected <- as.double(data$wt) + 1
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


## 'make_outcome_offset_matrices' ---------------------------------------------

test_that("'make_outcome_offset_matrices' works with model with offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = 1:2)
  data$age[c(1, 41)] <- NA
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_outcome_offset_matrices(mod, aggregate = TRUE)
  data_ag <- aggregate(data[c("deaths", "popn")], data[c("age", "region", "sex")], sum)
  data_ag <- data_ag[with(data_ag, order(age, sex, region)), ]
  ans_expected <- list(outcome = data_ag[["deaths"]],
                       offset = data_ag[["popn"]],
                       matrices_effect_outcome = make_matrices_effect_outcome(data_ag,
                                                                              mod$dimnames_terms),
                       matrix_covariates = matrix(NA_real_, nrow = 0, ncol = 0))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_outcome_offset_matrices' works with model without offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = 1:2)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  ans_obtained <- make_outcome_offset_matrices(mod, aggregate = TRUE)
  data_ag <- aggregate(data["deaths"], data[c("age", "sex", "region")], sum)
  ans_expected <- list(outcome = data_ag[["deaths"]],
                       offset = rep(1, times = nrow(data_ag)),
                       matrices_effect_outcome = make_matrices_effect_outcome(data_ag,
                                                                              mod$dimnames_terms),
                       matrix_covariates = matrix(NA_real_, nrow = 0, ncol = 0))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_outcome_offset_matrices' works with model with offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = 1:2)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$popn[1] <- 0
  data$deaths[1] <- 0
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_outcome_offset_matrices(mod, aggregate = FALSE)
  ans_expected <- list(outcome = mod$outcome[-1],
                       offset = mod$offset[-1],
                       matrices_effect_outcome = make_matrices_effect_outcome(data[-1,],
                                                                              mod$dimnames_terms),
                       matrix_covariates = matrix(NA_real_, nrow = 0, ncol = 0))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_outcome_offset_matrices' works with model with offset", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = 1:2)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$deaths[1] <- NA
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = 1)
  ans_obtained <- make_outcome_offset_matrices(mod, aggregate = FALSE)
  ans_expected <- list(outcome = mod$outcome[-1],
                       offset = mod$offset[-1],
                       matrices_effect_outcome = make_matrices_effect_outcome(data[-1,],
                                                                              mod$dimnames_terms),
                                              matrix_covariates = matrix(NA_real_, nrow = 0, ncol = 0))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_outcome_offset_matrices' works with model with numeric covariates", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = 1:2)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_covariates(~ income)
  ans_obtained <- make_outcome_offset_matrices(mod, aggregate = TRUE)
  data_ag <- make_data_df(mod)
  outcome_df <- aggregate(data_ag["deaths"], data_ag[c("age", "sex", "region", "income")], sum)
  offset_df <- aggregate(data_ag["popn"], data_ag[c("age", "sex", "region", "income")], sum)
  data_ag <- merge(outcome_df, offset_df, by = c("age", "sex", "region", "income"))
  ans_expected <- list(outcome = data_ag[["deaths"]],
                       offset = data_ag[["popn"]],
                       matrices_effect_outcome = make_matrices_effect_outcome(data_ag,
                                                                              mod$dimnames_terms),
                       matrix_covariates = matrix(scale(data_ag$income),
                                                  nrow = nrow(data),
                                                  dimnames = list(NULL, "income")))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'make_outcome_offset_matrices' works with model with categorical covariates", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = as.character(1:3))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  data$income <- rnorm(n = nrow(data))
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn) |>
    set_covariates(~ time)
  ans_obtained <- make_outcome_offset_matrices(mod, aggregate = TRUE)
  data_ag <- make_data_df(mod)
  outcome_df <- aggregate(data_ag["deaths"], data_ag[c("age", "sex", "region", "time")], sum)
  offset_df <- aggregate(data_ag["popn"], data_ag[c("age", "sex", "region", "time")], sum)
  data_ag <- merge(outcome_df, offset_df, by = c("age", "sex", "region", "time"))
  ans_expected <- list(outcome = data_ag[["deaths"]],
                       offset = data_ag[["popn"]],
                       matrices_effect_outcome = make_matrices_effect_outcome(data_ag,
                                                                              mod$dimnames_terms),
                       matrix_covariates = cbind(time2 = rep(c(0L, 1L, 0L), times = 40),
                                                 time3 = rep(c(0L, 0L, 1L), times = 40)))
  expect_equal(ans_obtained, ans_expected)
})


## 'make_prior_class' ---------------------------------------------------------

test_that("'make_prior_class' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:5,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  mod <- mod_pois(deaths ~ age * sex + region,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_prior_class(mod)
  ans_expected <- tibble::tibble(term = c("(Intercept)", "age", "sex", "region", "age:sex"),
                                 class = c("bage_prior_normfixed",
                                           "bage_prior_rwrandom",
                                           "bage_prior_normfixed",
                                           "bage_prior_norm",
                                           "bage_prior_rwrandom"))
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


## 'make_seed' --------------------------------------------------------------

test_that("'make_seed' returns a single unique integer", {
    set.seed(0)
    ans1 <- make_seed()
    ans2 <- make_seed()
    expect_true(is.integer(ans1))
    expect_identical(length(ans1), 1L)
    expect_false(ans1 == ans2)
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
    ans_expected <- factor(c("(Intercept)", rep("agegp", 5),
                             "SEX", "region", "agegp:SEX", "agegp:SEX"),
                           levels = c("(Intercept)", "agegp", "SEX", "region", "agegp:SEX"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_effects' -----------------------------------------------------------

test_that("'make_terms_effects' works with valid inputs", {
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
  ans_obtained <- make_terms_effects(mod$dimnames_terms)
  ans_expected <- factor(c("(Intercept)", rep("agegp", 10), rep("SEX", 2),
                           rep("region", 2), rep("agegp:SEX", 20)),
                         levels = c("(Intercept)", "agegp", "SEX", "region", "agegp:SEX"))
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
                    exposure = popn) |>
      set_prior(agegp ~ RW(sd = 0))
    ans_obtained <- make_terms_effectfree(mod)
    ans_expected <- factor(c("(Intercept)",
                             rep("agegp", times = 9),
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


## 'make_terms_hyperrandfree' -------------------------------------------------

test_that("'make_terms_hyperrandfree' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age + sex*time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- set_prior(mod, sex:time ~ Lin())
  ans_obtained <- make_terms_hyperrandfree(mod)
  ans_expected <- factor(rep("sex:time", 2),
                         levels = c("(Intercept)",
                                    "age",
                                    "sex",
                                    "time",
                                    "sex:time"))
  expect_identical(ans_obtained, ans_expected)                      
})


## 'make_use_term' ------------------------------------------------------------

test_that("'make_use_term' works", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:3,
                      sex = c("F", "M"),
                      time = 1:3,
                      deaths = 3)
  mod <- mod_pois(deaths ~ age * sex + age * region + sex * time,
                  data = data,
                  exposure = 1)
  vars_inner <- c("sex", "age")
  ans_obtained <- make_use_term(mod = mod, vars_inner = vars_inner)
  ans_expected <- c(T, T, T, F, F, T, F, F)
  names(ans_expected) <- names(mod$priors)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_use_term' throws correct error when 'vars_inner' has invalid variable", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:3,
                      sex = c("F", "M"),
                      time = 1:3,
                      deaths = 3)
  mod <- mod_pois(deaths ~ age * sex + age * region + sex * time,
                  data = data,
                  exposure = 1)
  vars_inner <- c("sex", "wrong")
  expect_error(make_use_term(mod = mod, vars_inner = vars_inner),
               "`vars_inner` has variable not found in model.")
})

test_that("'make_use_term' throws correct error when cannot form term from 'vars_inner'", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:3,
                      sex = c("F", "M"),
                      time = 1:3,
                      deaths = 3)
  mod <- mod_pois(deaths ~ age : sex + age : region + sex * time,
                  data = data,
                  exposure = 1)
  vars_inner <- "age"
  expect_error(make_use_term(mod = mod, vars_inner = vars_inner),
               "No terms in model can be formed from `vars_inner`.")
})

test_that("'make_use_term' throws correct error when can form all terms from 'vars_inner'", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:3,
                      sex = c("F", "M"),
                      time = 1:3,
                      deaths = 3)
  mod <- mod_pois(deaths ~ age : sex + age : region + sex * time,
                  data = data,
                  exposure = 1)
  vars_inner <- c("age", "sex", "region", "time")
  expect_error(make_use_term(mod = mod, vars_inner = vars_inner),
               "All terms in model can be formed from `vars_inner`.")
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


## 'make_uses_hyperrandfree' --------------------------------------------------

test_that("'make_uses_hyperrandfree' works", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + sex*time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_prior(mod, sex:time ~ Lin())
    ans_obtained <- make_uses_hyperrandfree(mod)
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
                      "agegp:SEX" = 1L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_uses_matrix_effectfree_effect' ---------------------------------------

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



## 'make_vars_inner' ----------------------------------------------------------

test_that("'make_vars_inner' works with age, sex, time present", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = 1:2)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + region + time
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  ans_obtained <- make_vars_inner(mod)
  ans_expected <- c("age", "sex", "time")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_vars_inner' works with age, sex present", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = 1:2)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex + region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_message(make_vars_inner(mod),
                 "Setting `vars_inner` to \"age\" and \"sex\".")
  ans_obtained <- suppressMessages(make_vars_inner(mod))
  ans_expected <- c("age", "sex")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'make_vars_inner' throws correct error with age, sex, time not present", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:2,
                      sex = c("F", "M"),
                      time = 1:2)
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ region
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  expect_error(make_vars_inner(mod),
               "Unable to infer `vars_inner`.")
})


## 'message_suspicious_rates' -------------------------------------------------

test_that("'message_suspicious_rates' returns NULL with valid inputs", {
  outcome <- c(0:40, NA)
  exposure <- c(0.000001, rep(3, 41))
  ans_obtained <- message_suspicious_rates(outcome = outcome,
                                           exposure = exposure,
                                           mult_high_rate = 1000)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
})

test_that("'message_suspicious_rates' returns NULL with less than 20 obs", {
  outcome <- 1
  exposure <- 0.00000001
  ans_obtained <- message_suspicious_rates(outcome = outcome,
                                           exposure = exposure,
                                           mult_high_rate = 1000)
  ans_expected <- NULL
  expect_identical(ans_obtained, ans_expected)
})

test_that("'message_suspicious_rates' returns message with 1 row", {
  outcome <- rep(1, 21)
  exposure <- c(rep(10, 20), 0.00000001)
  suppressMessages(
    expect_message(message_suspicious_rates(outcome = outcome,
                                            exposure = exposure,
                                            mult_high_rate = 1000),
                   "`data` has row with unexpectedly high rate.")
  )
})

test_that("'message_suspicious_rates' returns message with 2 rows", {
  outcome <- rep(1, 43)
  exposure <- c(rep(10, 40), NA, 0.00000001, 0.0000002)
  suppressMessages(
    expect_message(message_suspicious_rates(outcome = outcome,
                                            exposure = exposure,
                                            mult_high_rate = 100),
                   "`data` has rows with unexpectedly high rates.")
  )
})




## 'n_col' --------------------------------------------------------------------

test_that("'n_col' works with ordinary matrix", {
  m <- matrix(1:6, nr = 2)
  expect_identical(n_col(m), 3L)
})

test_that("'n_col' works with Matrix matrix", {
  m <- Matrix::Matrix(1:6, nr = 2)
  expect_identical(n_col(m), 3L)
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


## 'reduce_model_terms' -------------------------------------------------------

test_that("'reduce_model' works with excluding non-intercept terms", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:3,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  set.seed(1)
  mod <- mod_pois(deaths ~ age * sex + sex * region,
                  data = data,
                  exposure = popn)
  use_term <- make_use_term(mod, vars_inner = c("age", "sex"))
  ans_obtained <- reduce_model_terms(mod, use_term = use_term)
  set.seed(1) ## needed because mod_pois sets random seed
  ans_expected <- mod_pois(deaths ~ age * sex,
                           data = data,
                           exposure = popn)
  expect_identical(ans_expected$formula, deaths ~ age * sex)
  ans_expected$formula <- ans_obtained$formula
  expect_identical(ans_obtained, ans_expected)
})

test_that("'reduce_model' works with excluding intercept", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:3,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  set.seed(1)
  mod <- mod_pois(deaths ~ age * sex + sex * region,
                  data = data,
                  exposure = popn)
  use_term <- make_use_term(mod, vars_inner = c("age", "sex"))
  use_term[[1]] <- FALSE
  set.seed(1)
  ans_no_intercept <- reduce_model_terms(mod, use_term = use_term)
  expect_false("(Intercept)" %in% names(ans_no_intercept$priors))
  expect_identical(deparse(ans_no_intercept$formula), "deaths ~ age + sex + age:sex - 1")
})

test_that("'reduce_model' works intercept is only term", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      region = 1:3,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  set.seed(1)
  mod <- mod_pois(deaths ~ age * sex + sex * region,
                  data = data,
                  exposure = popn)
  use_term <- make_use_term(mod, vars_inner = c("age", "sex"))
  use_term[[1]] <- TRUE
  use_term[-1] <- FALSE
  set.seed(1)
  ans_intercept_only <- reduce_model_terms(mod, use_term = use_term)
  expect_identical(names(ans_intercept_only$priors), "(Intercept)")
  expect_identical(deparse(ans_intercept_only$formula), "deaths ~ 1")
})


## 'set_priors_known' ---------------------------------------------------------

test_that("'set_priors_known' works with valid inputs", {
  set.seed(0)
  data <- expand.grid(age = 0:9,
                      sex = c("F", "M"))
  data$popn <- rpois(n = nrow(data), lambda = 100)
  data$deaths <- rpois(n = nrow(data), lambda = 10)
  formula <- deaths ~ age * sex
  mod <- mod_pois(formula = formula,
                  data = data,
                  exposure = popn)
  mod <- fit(mod)
  prior_values <- make_point_est_effects(mod)
  ans_obtained <- set_priors_known(mod, prior_values = prior_values)
  ans_expected <- unfit(mod)
  ans_expected$priors[[1]] <- Known(prior_values[["(Intercept)"]])
  ans_expected$priors[[2]] <- Known(prior_values[["age"]])
  ans_expected$priors[[3]] <- Known(prior_values[["sex"]])
  ans_expected$priors[[4]] <- Known(prior_values[["age:sex"]])
  expect_equal(ans_obtained, ans_expected)
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
  ans_expected <- c("s=0.5", "", "", "", "")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_svd' works - AR", {
  prior <- AR(n_coef = 3, shape1 = 2, shape2 = 2)
  ans_obtained <- str_call_args_ar(prior)
  ans_expected <- c("n_coef=3", "", "shape1=2", "shape2=2")
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_con' --------------------------------------------------------

test_that("'str_call_args_con' works - con is 'none'", {
  prior <- RW()
  ans_obtained <- str_call_args_con(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_con' works - con is 'by'", {
  prior <- RW(con = "by")
  ans_obtained <- str_call_args_con(prior)
  ans_expected <- 'con="by"'
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_lin' --------------------------------------------------------

test_that("'str_call_args_lin' works - mean_slope = 0, sd_slope = 1", {
  prior <- Lin()
  ans_obtained <- str_call_args_lin(prior)
  ans_expected <- c("", "")
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_lin' works - sd_slope not 1", {
  prior <- Lin(sd = 0.3, mean_slope = -0.02)
  ans_obtained <- str_call_args_lin(prior)
  ans_expected <- c("mean_slope=-0.02", "sd_slope=0.3")
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

test_that("'str_call_args_s_seas' works", {
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


## 'str_call_args_sd' -----------------------------------------------------

test_that("'str_call_args_sd' works - sd = 1", {
  prior <- RW_Seas(n=3)
  ans_obtained <- str_call_args_sd(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_sd' works - sd not 1", {
  prior <- RW_Seas(n=2,sd = 0.3)
  ans_obtained <- str_call_args_sd(prior)
  ans_expected <- "sd=0.3"
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_sd_seas' -----------------------------------------------------

test_that("'str_call_args_sd_seas' works - sd_seas = 1", {
  prior <- RW_Seas(n=3)
  ans_obtained <- str_call_args_sd_seas(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_sd_seas' works - sd_seas not 1", {
  prior <- RW_Seas(n=2,sd_seas = 0.3)
  ans_obtained <- str_call_args_sd_seas(prior)
  ans_expected <- "sd_seas=0.3"
  expect_identical(ans_obtained, ans_expected)
})


## 'str_call_args_sd_slope' ---------------------------------------------------

test_that("'str_call_args_sd_slope' works - sd_slope = 1", {
  prior <- RW2()
  ans_obtained <- str_call_args_sd_slope(prior)
  ans_expected <- ""
  expect_identical(ans_obtained, ans_expected)
})

test_that("'str_call_args_sd_slope' works - non-default", {
  prior <- RW2(sd_slope = 0.2)
  ans_obtained <- str_call_args_sd_slope(prior)
  ans_expected <- "sd_slope=0.2"
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


