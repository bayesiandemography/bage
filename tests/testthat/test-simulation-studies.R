
## Poisson, with exposure, Lin_AR1 and SVD

if (FALSE) {

  set.seed(0)
  data <- expand.grid(age = poputils::age_labels(type = "lt"),
                      sex = c("Female", "Male"),
                      time = 2001:2010)
  data$population <- runif(n = nrow(data), min = 100, max = 300)
  data$deaths <- NA
  mod_est <- mod_pois(deaths ~ age : sex + sex * time,
                      data = data,
                      exposure = population) |>
  set_prior(`(Intercept)` ~ NFix(s = 0.01)) |>
  set_prior(age:sex ~ SVD(HMD)) |>
  set_prior(time ~ Lin_AR1(s = 0.1)) |>
  set_prior(sex:time ~ RW(s = 0.01)) |>
  set_prior(sex ~ NFix(sd = 0.1)) |>
  set_disp(mean = 0.05)
  rep <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
  
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
  data <- expand.grid(age = poputils::age_labels(type = "lt"),
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
  rep <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
  
}
