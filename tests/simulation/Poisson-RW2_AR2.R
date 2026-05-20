
library(bage)
set.seed(0)

data <- expand.grid(age = poputils::age_labels(type = "lt"),
                    sex = c("Female", "Male"),
                    time = 2001:2015)
data$population <- runif(n = nrow(data), min = 100, max = 300)
mod_est <- mod_pois(deaths ~ age : sex + sex * time,
                    data = data,
                    exposure = population) |>
  set_prior(`(Intercept)` ~ NFix(s = 0.01)) |>
  set_prior(age:sex ~ RW(s = 0.02)) |>
  set_prior(time ~ RW2_AR(s_rw = 0.01, sd = 0.02, sd_slope = 0.01, s_ar = 0.01)) |>
  set_prior(sex:time ~ RW(s = 0.01)) |>
  set_prior(sex ~ NFix(sd = 0.1)) |>
  set_disp(mean = 0.01)
rep <- report_sim(mod_est = mod_est, n_sim = 100, n_core = 10)
print(rep)



