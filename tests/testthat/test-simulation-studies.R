
## Poisson, with exposure, Lin_AR1 and SVD

if (FALSE) {

  set.seed(12)
  data <- expand.grid(age = poputils::age_labels(type = "lt"),
                      sex = c("Female", "Male"),
                      time = 2001:2015)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- NA
  mod_est <- mod_pois(deaths ~ age : sex + sex * time,
                      data = data,
                      exposure = population) |>
  set_prior(`(Intercept)` ~ NFix(s = 0.01)) |>
  set_prior(age:sex ~ SVD(HMD)) |>
  set_prior(time ~ RW(s = 0.05)) |>
  set_prior(sex:time ~ RW(s = 0.01)) |>
  set_prior(sex ~ NFix(sd = 0.1)) |>
  set_disp(mean = 0.05)
  rep <- report_sim(mod_est = mod_est, n_sim = 1000, n_core = 10, report_type = "full")
  
}


## Binomial, SVD_AR1 and SVD

if (FALSE) {

  set.seed(100)
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      sex = c("Female", "Male"),
                      time = 2001:2050)
  data$population <- round(runif(n = nrow(data), min = 1000, max = 3000))
  data$deaths <- NA
  mod_est <- mod_binom(deaths ~ age:sex + time,
                       data = data,
                       size = population) |>
  set_prior(`(Intercept)` ~ NFix(s = 0.01)) |>
  set_prior(age:sex ~ SVD(LFP)) |>
  set_prior(time ~ AR1(s = 0.01)) |>
  set_disp(mean = 0.05) 
  rep <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
  
}

## Normal

if (FALSE) {

  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      sex = c("Female", "Male"),
                      time = 2001:2005)
  data$income <- NA
  mod_est <- mod_norm(income ~ age + sex + time,
                      data = data,
                      weights = 1)
  mod_est <- set_prior(mod_est, age ~ SVD(LFP))
  mod_est <- set_prior(mod_est, time ~ AR1())
  mod_est <- set_disp(mod_est, mean = 0.05)
  rep <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
  
}


## RR3

if (FALSE) {

  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "single"),
                      sex = c("Female", "Male"))
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- 3
  mod_est <- mod_pois(deaths ~ age : sex,
                      data = data,
                      exposure = population) |>
  set_prior(`(Intercept)` ~ NFix(s = 0.01)) |>
  set_prior(age:sex ~ SVD(HMD)) |>
  set_disp(mean = 0.01) |>
  set_datamod_outcome_rr3()
  rep <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10, report_type = "short")
  
}


## covariates

if (FALSE) {

  set.seed(12)
  data <- expand.grid(age = poputils::age_labels(type = "lt"),
                      sex = c("Female", "Male"),
                      time = 2001:2015)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- NA
  data$income <- rnorm(n = nrow(data))
  data$is_2024_male <- data$age == 2024 & data$sex == "Male"
  mod_est <- mod_pois(deaths ~ age : sex + sex * time,
                      data = data,
                      exposure = population) |>
    set_prior(`(Intercept)` ~ NFix(s = 0.01)) |>
    set_prior(age:sex ~ SVD(HMD)) |>
    set_prior(time ~ RW(s = 0.05)) |>
    set_prior(sex:time ~ RW(s = 0.01)) |>
    set_prior(sex ~ NFix(sd = 0.1)) |>
    set_disp(mean = 0.05) |>
    set_covariates(~ income + is_2024_male)
  
  rep <- report_sim(mod_est = mod_est, n_sim = 1000, n_core = 10)
  
}



## var_inner

if (FALSE) {

  set.seed(0)

  data <- expand.grid(age = poputils::age_labels(type = "single"),
                      sex = c("Female", "Male"),
                      time = 2011:2023,
                      region = 1:330)
  data$population <- runif(n = nrow(data), min = 10, max = 1000)
  data$deaths <- NA

  mod_sim <- mod_pois(deaths ~ age * sex + region + time,
                      data = data,
                      exposure = population) |>
  set_prior(`(Intercept)` ~ NFix(s = 0.1)) |>
  set_prior(age ~ RW(s = 0.02)) |>
  set_prior(sex ~ NFix(sd = 0.1)) |>
  set_prior(age:sex ~ SVD(HMD)) |>
  set_prior(time ~ Lin_AR(s = 0.05, sd = 0.02)) |>
  set_prior(region ~ NFix(sd = 0.05)) |>
  set_disp(mean = 0.005)
  data_sim <- mod_sim |>
  set_n_draw(n_draw = 1) |>
  augment() |>
  select(age, sex, time, region, population, deaths) |>
  mutate(deaths = rvec::draws_median(deaths))


  mod_est <- mod_pois(deaths ~ age * sex + region + time,
                      data = data_sim,
                      exposure = population) |>
  set_prior(age:sex ~ SVD(HMD)) |>
  set_prior(time ~ Lin_AR()) |>
  set_prior(region ~ NFix(sd = 0.05)) |>
  set_prior(region:time ~ NFix(sd = 0.05)) |>
  set_prior(region:age ~ NFix(sd = 0.05))
  
  mod_est <- mod_est |>
  set_n_draw(n_draw = 10) |>
  fit(method = "inner-outer")
  
  system.time(
    rep <- report_sim(mod_est = mod_est,
                      mod_sim = mod_sim,
                      n_sim = 1,
                      n_core = 1)
  )
  
}



if (FALSE) {

  set.seed(330)
  
  data <- expand.grid(age = poputils::age_labels(type = "single"),
                      sex = c("Female", "Male"),
                      time = 2011:2023,
                      region = 1:100)
  data$population <- runif(n = nrow(data), min = 1, max = 100)
  data$deaths <- NA
  mod_est <- mod_pois(deaths ~ age : sex + region * time,
                      data = data,
                      exposure = population) |>
  set_prior(`(Intercept)` ~ NFix(s = 0.1)) |>
  set_prior(age:sex ~ SVD(HMD)) |>
  set_prior(time ~ Lin_AR1(sd = 0.02, s = 0.1)) |>
  set_prior(region ~ N(s = 0.05)) |>
  set_disp(mean = 0.02) |>
  set_n_draw(n_draw = 100)
  system.time(
    rep <- report_sim(mod_est = mod_est,  vars_inner = c("age", "sex", "time"), n_sim = 5, n_core = 1)
  )
  
}
