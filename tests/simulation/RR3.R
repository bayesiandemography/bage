
library(bage)
set.seed(0)

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
rep <- report_sim(mod_est = mod_est, n_sim = 1000, n_core = 10)
print(rep)
