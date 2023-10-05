
## 'default_prior' ------------------------------------------------------------

test_that("'default_prior' works with ordinary term", {
    expect_identical(default_prior(nm_term = "x",
                                   var_age = "age",
                                   var_time = "time",
                                   length_par = 5L),
                     N())
})

test_that("'default_prior' works with intercept", {
    expect_identical(default_prior(nm_term = "(Intercept)",
                                   var_age = "age",
                                   var_time = "time",
                                   length_par = 1L),
                     NFix(sd = 10))
})

test_that("'default_prior' works with term with length 1", {
    expect_identical(default_prior(nm_term = "region",
                                   var_age = "age",
                                   var_time = "time",
                                   length_par = 1L),
                     NFix())
})

test_that("'default_prior' works with age term", {
    expect_identical(default_prior(nm_term = "AgeGroup",
                                   var_age = "AgeGroup",
                                   var_time = "time",
                                   length_par = 5),
                     RW())
    expect_identical(default_prior(nm_term = "AgeGroup",
                                   var_age = "AgeGroup",
                                   var_time = NULL,
                                   length_par = 5),
                     RW())
    expect_identical(default_prior(nm_term = "AgeGroup",
                                   var_age = NULL,
                                   var_time = NULL,
                                   length_par = 5),
                     N())
})

test_that("'default_prior' works with time term", {
    expect_identical(default_prior(nm_term = "year",
                                   var_age = "AgeGroup",
                                   var_time = "year",
                                   length_par = 5),
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
                     NULL)
    expect_identical(make_agesex_inner("(Intercept)",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "other")
    expect_identical(make_agesex_inner("agegroup:gender",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "age:sex")
    expect_identical(make_agesex_inner("agegroup:gender",
                                       var_age = "agegroup",
                                       var_sexgender = NULL),
                     NULL)
    expect_identical(make_agesex_inner("gender:agegroup",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "sex:age")
    expect_identical(make_agesex_inner("region:agegroup",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "other")
    expect_identical(make_agesex_inner("gender:agegroup:region",
                                       var_age = "agegroup",
                                       var_sexgender = "gender"),
                     "other")
    expect_identical(make_agesex_inner("gender:agegroup:region",
                                       var_age = NULL,
                                       var_sexgender = NULL),
                     "other")
})


## 'make_const' --------------------------------------------------------------- 

test_that("'make_const' works with valid inputs", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()))
    ans_obtained <- make_const(mod)
    ans_expected <- c(1, 1, 0.001, 1)
    expect_identical(ans_obtained, ans_expected)
    expect_true(is.double(ans_expected))
})


## 'make_const_season' -------------------------------------------------------- 

test_that("'make_const_season' works with non-NULL seasonal effect", {
    mod <- structure(list(scale_season = 1.3, n_season = 3L),
                     class = "bage_mod")
    ans_obtained <- make_const_season(mod)
    ans_expected <- c(1.3, 0.0001)
    expect_identical(ans_obtained, ans_expected)
    expect_true(is.double(ans_expected))
})

test_that("'make_const_season' works with non-NULL seasonal effect", {
    mod <- structure(list(scale_season = NULL, n_season = 0L),
                     class = "bage_mod")
    ans_obtained <- make_const_season(mod)
    ans_expected <- c(0, 0.0001)
    expect_identical(ans_obtained, ans_expected)
    expect_true(is.double(ans_expected))
})


## 'make_hyper' ---------------------------------------------------------------

test_that("'make_hyper' works with valid inputs", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()))
    ans_obtained <- make_hyper(mod)
    ans_expected <- rep(0, 3L)
    expect_identical(ans_obtained, ans_expected)
    expect_true(is.double(ans_expected))
})


## 'make_hyper_season' ---------------------------------------------------------------

test_that("'make_hyper_season' works with valid inputs", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()))
    ans_obtained <- make_hyper_season(mod)
    ans_expected <- 0
    expect_identical(ans_obtained, ans_expected)
    expect_true(is.double(ans_expected))
})


## 'make_i_prior' -------------------------------------------------------------

test_that("'make_i_prior' works with valid inputs", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()))
    ans_obtained <- make_i_prior(mod)
    ans_expected <- c(a = 1L, b = 3L, c = 1L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_idx_time' ------------------------------------------------------------

test_that("'make_idx_time' works with var_time non-NULL", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()),
                var_time = "b")
    expect_identical(make_idx_time(mod), 2L)
})

test_that("'make_idx_time' returns 0 when var_time NULL", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()),
                var_time = NULL)
    expect_identical(make_idx_time(mod), 0L)
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


## 'make_lengths_par' ---------------------------------------------------------

test_that("'make_lengths_par' works with valid inputs", {
    matrices_par_outcome = list(a = matrix(nr = 100, nc = 1),
                                b = matrix(nr = 100, nc = 5),
                                c = matrix(nr = 100, nc = 5))
    ans_obtained <- make_lengths_par(matrices_par_outcome)
    ans_expected <- c(a = 1L, b = 5L, c = 5L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_lengths_parfree' -----------------------------------------------------------

test_that("'make_lengths_parfree' works with valid inputs", {
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
    ans_obtained <- make_lengths_parfree(mod)
    ans_expected <- c("(Intercept)" = 1L,
                      agegp = 10L,
                      SEX = 2L,
                      region = 2L,
                      "agegp:SEX" = 20L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_levels_par' ----------------------------------------------------------

test_that("'make_levels_par' works with valid inputs - pois, complete levels", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age * sex + time
    outcome <- make_outcome(formula = formula, data = data)
    matrices_par_outcome <- make_matrices_par_outcome(formula = formula, data = data)
    ans_obtained <- make_levels_par(formula = formula,
                                    matrices_par_outcome = matrices_par_outcome,
                                    outcome = outcome,
                                    data = data)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'make_levels_par' works with valid inputs - pois, incomplete levels", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    data <- data[-3, ]
    formula <- deaths ~ age * sex + time
    outcome <- make_outcome(formula = formula, data = data)
    matrices_par_outcome <- make_matrices_par_outcome(formula = formula, data = data)
    ans_obtained <- make_levels_par(formula = formula,
                                    matrices_par_outcome = matrices_par_outcome,
                                    outcome = outcome,
                                    data = data)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'make_levels_par' works with valid inputs - norm", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$income <- rnorm(n = nrow(data))
    formula <- income ~ age * sex + time
    outcome <- make_outcome(formula = formula, data = data)
    matrices_par_outcome <- make_matrices_par_outcome(formula = formula, data = data)
    ans_obtained <- make_levels_par(formula = formula,
                                    matrices_par_outcome = matrices_par_outcome,
                                    outcome = outcome,
                                    data = data)
    ans_expected <- c("(Intercept)",
                      0:9,
                      c("F", "M"),
                      2000:2005,
                      paste(rep(0:9, times = 2),
                            rep(c("F", "M"), each = 10),
                            sep = "."))
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
    mod <- set_season(mod, n = 2, s = 0.2)
    ans_obtained <- make_map(mod)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_map' works when 'parfree' contains known values", {
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
    mod <- set_season(mod, n = 2)
    ans_obtained <- make_map(mod)
    ans_expected <- list(parfree = factor(c("(Intercept)" = 1,
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
    mod <- set_season(mod, n = 2, s = 0.2)
    mod <- set_disp(mod, s = 0)
    ans_obtained <- make_map(mod)
    ans_expected <- list(log_disp = factor(NA))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_map' works when there is no season effect", {
    set.seed(0)
    data <- expand.grid(time = 0:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time * SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_map(mod)
    ans_expected <- list(par_season = factor(logical()),
                         hyper_season = factor(NA))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_map' works when parfree has known values and there is no season effect", {
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
    ans_expected <- list(parfree = factor(c("(Intercept)" = 1,
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
                                            "time:SEX" = 13)),
                         par_season = factor(logical()),
                         hyper_season = factor(NA))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_map_hyper_season_fixed' ----------------------------------------------

test_that("'make_map_hyper_season_fixed' works", {
    mod <- list()
    expect_identical(make_map_hyper_season_fixed(mod), factor(NA))
    expect_identical(length(make_map_hyper_season_fixed(mod)),
                     length(make_hyper_season(mod)))
})


## 'make_map_par_season_fixed' ------------------------------------------------

test_that("'make_map_par_season_fixed' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(time = 0:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_map_par_season_fixed(mod)
    ans_expected <- factor(logical())
    expect_identical(ans_obtained, ans_expected)
    expect_identical(length(make_map_par_season_fixed(mod)),
                     length(make_par_season(mod)))
})


## 'make_map_parfree_fixed' ---------------------------------------------------

test_that("'make_map_par_season_fixed' works with valid inputs", {
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
    ans_obtained <- make_map_parfree_fixed(mod)
    ans_expected <- factor(c("(Intercept)" = 1,
                             time = 2,
                             time = 3,
                             time = 4,
                             SEX = NA,
                             SEX = NA))
    expect_identical(ans_obtained, ans_expected)
    expect_identical(length(make_map_parfree_fixed(mod)),
                     length(make_parfree(mod)))
})


## 'make_matrices_par_outcome' --------------------------------------------

test_that("'make_matrices_par_outcome' works with valid inputs", {
    data <- expand.grid(age = 0:5, time = 2000:2001, sex = 1:2)
    data$val <- 1
    data <- data[-c(3, 5), ]
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_matrices_par_outcome(formula = formula, data = data)
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


## 'make_matrices_parfree_par' ------------------------------------------------

test_that("'make_matrices_parfree_par' works with valid inputs", {
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
    ans_obtained <- make_matrices_parfree_par(mod)
    ans_expected <- list("(Intercept)" = Matrix::.sparseDiagonal(1),
                         agegp = Matrix::.sparseDiagonal(10),
                         SEX = Matrix::.sparseDiagonal(2),
                         region = Matrix::.sparseDiagonal(2),
                         "agegp:SEX" = Matrix::.sparseDiagonal(20))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_matrix_season_outcome' ------------------------------------------------------------

test_that("'make_matrix_season_outcome' works with bage_mod_pois - by is NULL", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_matrix_season_outcome(mod, by = NULL)
    ans_expected <- mod$matrices_par_outcome[["time"]]
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_season_outcome' works with bage_mod_pois - by is 1-dimensional", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + age:time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_matrix_season_outcome(mod, by = "age")
    ans_expected <- mod$matrices_par_outcome[["age:time"]]
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_season_outcome' works with bage_mod_pois - by is 2-dimensional", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age:sex + age:sex:time
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_matrix_season_outcome(mod, by = c("age", "sex"))
    ans_expected <- mod$matrices_par_outcome[["age:sex:time"]]
    expect_identical(ans_obtained, ans_expected)
})
    
test_that("'make_matrix_season_outcome' works with bage_mod_norm - by is NULL", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data <- data[-2,]
    data$income <- rnorm(n = nrow(data), mean = 10)
    formula <- income ~ age:sex + time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    ans_obtained <- make_matrix_season_outcome(mod, by = NULL)
    ans_expected <- mod$matrices_par_outcome[["time"]]
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_season_outcome' works with bage_mod_norm - by is 1-dim", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data <- data[-2,]
    data$income <- rnorm(n = nrow(data), mean = 10)
    formula <- income ~ age:sex + sex:time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    ans_obtained <- make_matrix_season_outcome(mod, by = "sex")
    ans_expected <- mod$matrices_par_outcome[["sex:time"]]
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_matrix_season_outcome' works with bage_mod_norm - by is 1-dim", {
    set.seed(0)
    data <- expand.grid(age = 0:9, time = 2000:2005, sex = c("F", "M"))
    data <- data[-2,]
    data$income <- rnorm(n = nrow(data), mean = 10)
    formula <- income ~ age:sex + age:sex:time
    mod <- mod_norm(formula = formula,
                    data = data,
                    weights = 1)
    ans_obtained <- make_matrix_season_outcome(mod, by = c("age", "sex"))
    ans_expected <- mod$matrices_par_outcome[["age:sex:time"]]
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


## 'make_offsets_parfree_par' ------------------------------------------------

test_that("'make_offsets_parfree_par' works with valid inputs", {
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
    ans_obtained <- make_offsets_parfree_par(mod)
    ans_expected <- list("(Intercept)" = 0,
                         agegp = rep(0, 10),
                         SEX = rep(0, 2),
                         region = rep(0, 2),
                         "agegp:SEX" = rep(0, 20))
    ans_expected <- unlist(ans_expected)
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


## 'make_par_season' ----------------------------------------------------------

test_that("'make_par_season' works when has season effect, by is NULL", {
    set.seed(0)
    data <- expand.grid(time = 0:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2)
    ans_obtained <- make_par_season(mod)
    ans_expected <- c(0.0, 0.0, 0.0, 0.0)
    names(ans_expected) <- 0:3
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_par_season' works when has season effect, by has dim 1", {
    set.seed(0)
    data <- expand.grid(time = 0:3,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ time + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    mod <- set_season(mod, n = 2, by = "SEX")
    ans_obtained <- make_par_season(mod)
    ans_expected <- rep(0.0, 8)
    names(ans_expected) <- paste(c("F", "M"), rep(0:3, each = 2), sep = ".")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_par_season' works when no season", {
    set.seed(0)
    data <- expand.grid(age = 0:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ age + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn)
    ans_obtained <- make_par_season(mod)
    ans_expected <- double()
    expect_identical(ans_obtained, ans_expected)
})


## 'make_parfree' -------------------------------------------------------------

test_that("'make_parfree' works with valid inputs", {
    set.seed(0)
    data <- expand.grid(agegp = 0:2,
                        SEX = c("F", "M"))
    data$popn <- rpois(n = nrow(data), lambda = 100)
    data$deaths <- rpois(n = nrow(data), lambda = 10)
    formula <- deaths ~ agegp + SEX
    mod <- mod_pois(formula = formula,
                    data = data,
                    exposure = popn) %>%
        set_prior((Intercept) ~ Known(3))
    ans_obtained <- make_parfree(mod)
    ans_expected <- c("(Intercept)" = 3,
                      agegp = 0, agegp = 0, agegp = 0,
                      SEX = 0, SEX = 0)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_priors' --------------------------------------------------------------

test_that("'make_priors' works with valid inputs - has intercept", {
    formula <- deaths ~ age:sex + time
    ans_obtained <- make_priors(formula,
                                var_age = "age",
                                var_time = "time",
                                lengths_par = c(1L, 10L, 12L))
    ans_expected <- list("(Intercept)" = NFix(sd = 10),
                         time = RW(),
                         "age:sex" = N())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_priors' works with valid inputs - no intercept", {
    formula <- deaths ~ age:sex + time - 1
    ans_obtained <- make_priors(formula,
                                var_age = "age",
                                var_time = "time",
                                lengths_par = c(10L, 12L))
    ans_expected <- list(time = RW(),
                         "age:sex" = N())
    expect_identical(ans_obtained, ans_expected)
})


## 'make_random' --------------------------------------------------------------

test_that("'make_random' works when no season effect", {
    mod <- structure(list(n_season = 0L),
                     class = "bage_mod")
    expect_identical(make_random(mod), "parfree")
})

test_that("'make_random' works when has season effect", {
    mod <- structure(list(n_season = 2L),
                     class = "bage_mod")
    expect_identical(make_random(mod), c("parfree", "par_season"))
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
    m <- make_spline_matrix(length_par = 10, n_spline = 5)
    expect_equal(dim(m), c(10L, 5L))
    expect_equal(colSums(as.matrix(m)), rep(0, times = 5))
})


## 'make_submatrix' -----------------------------------------------------------

test_that("'make_submatrix' works - dimension in term", {
    ans_obtained <- make_submatrix(d = 4L, is_in = TRUE)
    ans_expected <- diag(4L)
    expect_identical(as.matrix(ans_obtained), ans_expected)
    expect_s4_class(ans_obtained, "sparseMatrix")
})

test_that("'make_submatrix' works - dimension not in term", {
    ans_obtained <- make_submatrix(d = 4L, is_in = FALSE)
    ans_expected <- matrix(1, nrow = 4, ncol = 1)
    expect_identical(as.matrix(ans_obtained), ans_expected)
    expect_s4_class(ans_obtained, "sparseMatrix")
})    


## 'make_terms_const' ---------------------------------------------------------

test_that("'make_terms_const' works with valid inputs", {
    mod <- list(priors = list(a = N(), b = RW(), c = Known(1:3), d = N()))
    ans_obtained <- make_terms_const(mod)
    ans_expected <- factor(c("a", "b", "b", "c", "d"), levels = c("a", "b", "c", "d"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_hyper' ---------------------------------------------------------

test_that("'make_terms_hyper' works with valid inputs", {
    mod <- list(priors = list(a = N(), b = RW(), c = N()))
    ans_obtained <- make_terms_hyper(mod)
    ans_expected <- factor(c("a", "b", "c"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_par' -----------------------------------------------------------

test_that("'make_terms_par' works with valid inputs", {
    matrices_par_outcome = list(a = matrix(nr = 100, nc = 1),
                                b = matrix(nr = 100, nc = 5),
                                c = matrix(nr = 100, nc = 5))
    ans_obtained <- make_terms_par(matrices_par_outcome)
    ans_expected <- factor(rep(c("a", "b", "c"),
                               times = c(1, 5, 5)))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_terms_parfree' -----------------------------------------------------------

test_that("'make_terms_parfree' works with valid inputs", {
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
    ans_obtained <- make_terms_parfree(mod)
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


## 'make_uses_hyper' ----------------------------------------------------------

test_that("'make_uses_hyper' works with valid inputs", {
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
        set_prior(agegp ~ NFix())
    ans_obtained <- make_uses_hyper(mod)
    ans_expected <- c("(Intercept)" = 0L,
                      agegp = 0L,
                      SEX = 1L,
                      region = 1L,
                      "agegp:SEX" = 1L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_uses_matrix_parfree_par' ---------------------------------------------

test_that("'make_uses_matrix_parfree_par' works with valid inputs", {
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
        set_prior(agegp ~ Spline())
    ans_obtained <- make_uses_matrix_parfree_par(mod)
    ans_expected <- c("(Intercept)" = 0L,
                      agegp = 1L,
                      SEX = 0L,
                      region = 0L,
                      "agegp:SEX" = 0L)
    expect_identical(ans_obtained, ans_expected)
})


## 'make_uses_matrix_parfree_par' ---------------------------------------------

test_that("'make_uses_offset_parfree_par' works with valid inputs", {
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
        set_prior(agegp ~ Spline())
    ans_obtained <- make_uses_offset_parfree_par(mod)
    ans_expected <- c("(Intercept)" = 0L,
                      agegp = 0L,
                      SEX = 0L,
                      region = 0L,
                      "agegp:SEX" = 0L)
    expect_identical(ans_obtained, ans_expected)
})
