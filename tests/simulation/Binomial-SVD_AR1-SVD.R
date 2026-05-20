
library(bage)
set.seed(0)

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
print(rep)

