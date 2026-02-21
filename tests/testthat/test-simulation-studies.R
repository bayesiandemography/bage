
run <- FALSE
set.seed(0)

## Poisson, with exposure, Lin_AR1 and SVD

if (run) {
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
  rep1 <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
}


## Binomial, SVD_AR1 and SVD

if (run) {
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
  rep2 <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
}

## Normal

if (run) {
  data <- expand.grid(age = poputils::age_labels(type = "five", min = 15, max = 60),
                      sex = c("Female", "Male"),
                      time = 2001:2005)
  data$income <- NA
  mod_est <- mod_norm(income ~ age + sex + time,
                      data = data)
  mod_est <- set_prior(mod_est, age ~ SVD(LFP))
  mod_est <- set_prior(mod_est, time ~ AR1())
  mod_est <- set_disp(mod_est, mean = 0.05)
  rep3 <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
}


## RR3


if (run) {
  set.seed(101)
  data <- expand.grid(age = poputils::age_labels(type = "single"),
                      sex = c("Female", "Male"),
                      region = 1:3)
  data$population <- runif(n = nrow(data), min = 0, max = 10)
  mod_est <- mod_pois(deaths ~ age : sex,
                      data = data,
                      exposure = population) |>
    set_prior(`(Intercept)` ~ NFix(s = 0.01)) |>
    set_prior(age:sex ~ AR1(s = 0.05)) |>
    set_disp(mean = 0.01) |>
    set_confidential_rr3()
  rep4 <- report_sim(mod_est = mod_est, n_sim = 1000, n_core = 10)
}


## covariates

if (run) {
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
  rep5 <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
}


if (run) {
  print("Simulation 1")
  print(rep1)
  print("\n\nSimulation 2")
  print(rep2)
  print("\n\nSimulation 3")
  print(rep3)
  print("\n\nSimulation 4")
  print(rep4)
  print("\n\nSimulation 5")
  print(rep5)
}


